library(tikzDevice)
library(dplyr)
library(cowplot)
library(ggplot2)
	theme_set(theme_bw()) ## FIXME: don't hardcode? (coded in plot.R?)
library(directlabels)
library(epigrowthfit) ## for parish data


args = commandArgs(trailingOnly=TRUE)
## names for sanity, not actually used
defaults = list(output="",
                exclude_years=NULL,
                corner_labs=TRUE,
                standAlone=TRUE,
                width=8,height=4,
                ## max outbreak time in years (chop 1665 time series)
                max_yrs=1.5,
                add_CI=FALSE
                )
output = if (length(args) < 1) paste0(rtargetname, ".tex") else args[1]
exclude_years = if (length(args) < 2) defaults[[2]] else eval(parse(text=args[2]))
corner_labs = if (length(args) < 3) defaults[[3]] else as.logical(args[3])
standAlone = if (length(args) < 4) defaults[[4]] else as.logical(args[4])
width = if (length(args) < 5) defaults[[5]] else as.numeric(args[5])
height = if (length(args) < 6) defaults[[6]] else as.numeric(args[6])
max_yrs = if (length(args) <7) defaults$max_yrs else as.numeric(args[7])
add_CI = if (length(args) <8) defaults$add_CI else as.numeric(args[8])


if (!interactive()) tikz(output, width=width, height=height,
                         standAlone=standAlone)

## make sure each outbreak year lasts for a max of max_yrs
## in particular, this chops off the second half of the 1665-1667
## outbreak year, which we don't want anyway
chop_time <-
    (. %>% group_by(outbreak.year)
        %>% filter((time-time[1])<max_yrs)
        %>% ungroup()
    )

## self-explanatory, I hope ...
dd_majors3 <- (dd_majors2
    %>% filter(!outbreak.year %in% exclude_years)
    %>% chop_time()
    %>% add_epoch()
)


## fake data for expanding time axes
## guessing at algorithm: rule is really "expand if tick mark missing"
## (1564, 1604), "and not too much if it's present")
dd_expand <- (dd_majors3
    %>% group_by(epoch,outbreak.year)
    %>% summarise(time=if (min(time) %% 1 <0.5 &&
                           diff(range(time))<1) floor(min(time)) else min(time))
)

## don't need to chop_time here because predictions are over shorter scales
pp_majors3 <- (pp_majors2
    %>% filter(!outbreak.year %in% exclude_years)
    %>% add_epoch())

## FIXME: process data to extend x-lims slightly lower;
##  always want first two years

## these won't be *exactly* correct because of leap years etc.
##  but who cares (we are already making much bigger approximations,
##  e.g. Julian/Gregorian)
april1_dec <- lubridate::decimal_date(as.Date("2018-04-01")) %% 1
july1_dec <- lubridate::decimal_date(as.Date("2018-07-01")) %% 1
oct1_dec <- lubridate::decimal_date(as.Date("2018-10-01")) %% 1
brks <- function(lims) {
    if (all(lims<1000)) return(NULL)
    ## print(lims)
    r <- round(lims)
    if (length(r)==1) { ## it's always beginning missing
        r <- c(r-1,r)
    } else {
        if (max_yrs<2)
            ## include only two year breaks
            ## not sure why chop_time didn't achieve this automatically ...
            r <- c(r[1],r[1]+1)
    }
    print(r)
    r
}

mbrks <- function(lims) {
    bb <- brks(lims)
    mb <- c(outer(bb,c(april1_dec,july1_dec,oct1_dec),"+"))
    mb <- sort(mb[lims[1] < mb & mb < lims[2]])
    return(mb)
}

## data for placing direct labels
tlabs <- (data.frame(outbreak.year="1625",
                     ## set order based on setcolours
                     source=factor(c("wills","parish","LBoM"),
                                   levels=names(colvec1)),
                     ## FIXME: fragile to changes in setcolours
                     time=c(wills=1625.5,parish=1625,LBoM=1625.5),
                     deaths=c(wills=5,parish=100,LBoM=200),
                     stringsAsFactors=FALSE)
    %>% add_epoch()
)

## data for placing outbreak-year labels in corner (more or less)
##  of each panel
clabs1 <- (dd_majors3
    ## to put labels at max for individual OUTBREAK, use a single
    ##  group_by(outbreak.year) here ...
    %>% group_by(epoch)
    %>% summarise(deaths=max(deaths,na.rm=TRUE)))
clabs <- full_join(dd_expand,clabs1,by="epoch")
    
## nudge label left
## clabs[clabs$outbreak.year=="1563","time"] <-
##     clabs[clabs$outbreak.year=="1563","time"] - 0.15

## add fake data to get a blank frame:
## need all three sources (so we get all three colours)
## any old time vector as long as it's <1000 and will get suppressed
## by 'fakelabs' below
## early epoch (since that's where we'll want our blank element
## by default this appears as leftmost panel -- could make it
## rightmost by changing factor levels so "" sorts at the end
dd_fake <- data.frame(source=levels(dd_majors3$source),
                      outbreak.year="",time=c(0,0.5,1),deaths=NA,
                      date=NA,
                      epoch="14th c.",
                      stringsAsFactors=TRUE)

fakelabs <- function(brks) {
    ## cat("fakelabs ",brks,"\n")
    if (all(brks>1000)) {
        as.character(brks)
    } else if (any(brks>1000)) {
        ifelse(brks>1000,as.character(brks),"")
    } else NULL
}

## function to plot timeseries/fits from a single epoch
mkfig <- function(dd=dd_majors3,   ## deaths/wills data
                  pp=pp_majors3,   ## predictions
                  ep=NULL,         ## epoch to plot
                  corner_labs=FALSE,  ## show outbreak years
                                      ## using corner instead of strip labels
                  ylab="deaths or wills\n(per week)",
                  ybrks = c(1,2,5,10,100,1000),
                  directlabs=TRUE) {
    if (!is.null(ep)) {
        ## filter data
        dd <- subset(dd,epoch==ep)
        dd_expand <- subset(dd_expand,epoch==ep)
        pp <- subset(pp,epoch==ep)
        clabs <- subset(clabs,epoch==ep)
        tlabs <- subset(tlabs,epoch==ep)
    }
    ## make sure levels match (ugh)
    fixlevels <- (. %>% mutate(outbreak.year=
                                   factor(outbreak.year,
                                          levels=levels(dd$outbreak.year))))
    dd_expand <- dd_expand %>% fixlevels
    pp <- pp %>% fixlevels
    clabs <- clabs %>% fixlevels
    tlabs <- tlabs %>% fixlevels
    fig <- (ggplot(dd, aes(time,deaths, colour=source))
        + geom_point(alpha=0.4,aes(shape=source))
        + geom_line(data=pp,size=1)
        + scale_y_log10(breaks=ybrks)
        + scale_colour_manual(values=colvec1,guide=guide_legend(reverse=TRUE))
        + scale_shape_manual(values=pchvec1,guide=guide_legend(reverse=TRUE))
        + labs(x="",y=ylab)
        + geom_point(data=dd_expand,aes(x=time),y=1,
                     colour=NA)
    )
    if (add_CI) {
        fig <- (fig
            + geom_ribbon(data=pp,
                          aes(fill=source,ymin=lower,ymax=upper),
                          colour=NA,alpha=0.2)
            + scale_fill_manual(values=colvec1,guide=FALSE)
        )
    }

    if (packageVersion("ggplot2")<="2.2.1") {
        fig <- fig + scale_x_continuous(breaks=brks,
                                        expand=c(0,0.2),
                                        labels=fakelabs)
    } else {
        fig <- fig + scale_x_continuous(breaks=brks,
                                        minor_breaks=mbrks,
                                        expand=expand_scale(add=0.2),
                                        labels=fakelabs)
    }
    if (corner_labs) {
        fig <- (fig
            + geom_text(data=clabs,
                        aes(label=outbreak.year),
                        colour="black",
                        hjust=0,
                        vjust=1)
        )
        if (directlabs && nrow(tlabs)>0) {
            fig <- fig + geom_text(data=tlabs,
                        aes(label=source,
                            hjust=0)) +
                theme(legend.position="none")
        }
        ## erase strip labels
        ## https://stackoverflow.com/questions/10547487/remove-facet-wrap-labels-completely
        fig <- fig + theme(strip.background=element_blank(),
                    strip.text.x=element_blank(),
                    strip.text.y=element_blank()
                    )
        
    }
    return(fig)
}

if (corner_labs) {
    ## fancy two-row format (poster, paper, etc.)
    dd <- dd_majors3
    if (!is.null(exclude_years)) {
        ## put legend at left edge of lower plot
        leg_pos_early <- "none"
        leg_pos_late <- c(0.01,0.8)
    } else {
        ## put legend at left edge of upper plot
        leg_pos_early <- c(0.9,0.5)
        leg_pos_late <- "none"
        all_yrs <- (dd_majors3
            %>% select(outbreak.year,epoch)
            %>% unique()
            %>% arrange(outbreak.year)
        )
        yrvec <- with(all_yrs,c(outbreak.year[epoch=="14th c."],
                                "",
                                outbreak.year[epoch=="16th-17th c."]))

        ## NB dd_expand is external/global
        ## extend dd to include all data within dd_expand period ...
        ep_oy <- (dd_majors3
            %>% filter(epoch=="16th-17th c.")
            %>% select(outbreak.year)
            %>% unique()
            %>% pull(outbreak.year)
        )
        get_parish_dat <- function(oy,debug=FALSE) {
            dd_t <- filter(dd,outbreak.year==oy,source=="parish") %>% pull(time)
            dde_t <- filter(dd_expand,outbreak.year==oy) %>% pull(time)
            res <- (london_parish_all
                %>% select(time,total)
                %>% mutate(source="parish",epoch="16th-17th c.",outbreak.year=oy)
                %>% rename(deaths=total)
                %>% filter(time>dde_t[1] & time<min(dd_t))
            )
            return(res)
        }
        extra <- bind_rows(lapply(ep_oy,get_parish_dat))
        ## FIXME: adjust char/factor so we don't have to suppress warnings/reset levels?
        dd <- (suppressWarnings(bind_rows(dd_majors3,extra,dd_fake)) %>%
               mutate(outbreak.year=factor(outbreak.year,levels=yrvec)))
    }
    fig_early <- (
        mkfig(dd=dd,
              ep="14th c.",corner_labs=TRUE,
              ylab="wills\n(per week)",directlabs=FALSE)
        + facet_grid(.~outbreak.year,scale="free_x")
        + theme(plot.margin=grid::unit(c(5.5,5.5,0,5.5),"pt"),
                panel.border=element_blank(),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_line(colour="gray",linetype=2),
                legend.position=leg_pos_early)
    )
    ## hack white rectangle to cover grid
    fig_early <- fig_early + geom_rect(data=dd_fake,
                          fill="white",
                          colour=NA,
                          ymin=-1,
                          ymax=10,
                          xmin=-1,
                          xmax=2)
    fig_late <- (mkfig(dd=dd,
                  ep="16th-17th c.",corner_labs=TRUE,
                       ybrks = c(1,10,100,1000),
                       directlabs=FALSE)
        + facet_grid(.~outbreak.year,scale="free_x")
        ## FIXME: DRY with theme as much ...
        + theme(plot.margin=grid::unit(c(0,5.5,5.5,5.5),"pt"),
                panel.border=element_blank(),
                panel.grid.minor.x=element_line(colour="gray",linetype=2),
                panel.grid.minor.y=element_blank(),
                legend.position=leg_pos_late)
    )
    fig <- plot_grid(fig_early,fig_late,nrow=2,align="v",
                     axis="lr")
} else {
    fig <- mkfig() + facet_wrap(~outbreak.year,scale="free")
}
print(fig)

if (!interactive()) dev.off()

(mkfig(dd=rbind(dd_majors3,dd_fake),
       ep="14th c.",corner_labs=TRUE,
       ylab="wills",directlabs=FALSE)
    +facet_grid(.~outbreak.year,scale="free_x")
    +theme(plot.margin=grid::unit(c(5.5,5.5,0,5.5),"pt"),
           panel.border=element_blank(),
           legend.position="none")
)

