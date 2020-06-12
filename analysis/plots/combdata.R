library(epigrowthfit)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(directlabels)
library(tikzDevice)
library(cowplot)
library(gtable)
library(glmmTMB)

## plot growth rate results from various different data sources
args <- commandArgs(trailingOnly=TRUE)

## args <- NULL ## interactive/debugging
defaults = list(output="",width=7,height=4,size=0.5,incl_ref=FALSE,
                ci_type="pointrange")

texname <- paste0(rtargetname, ".tex")

output = if (length(args) < 1) texname else args[1]
width = if (length(args) < 2) defaults[[2]] else as.numeric(args[2])
height = if (length(args) < 3) defaults[[3]] else as.numeric(args[3])
size =  if (length(args) < 4 ) defaults[[4]] else as.numeric(args[4])
incl_ref = if (length(args) < 5 ) defaults[[5]] else as.logical(args[5])
ci_type = if (length(args) < 6 ) defaults[[6]] else args[6]

hack_labs <- TRUE

cat(output,width,height,size,"\n")

## modify widths of specified columns
## (not used: messes up ggplot format)
hack_widths <- function(g,width=1,unit="cm",pattern="axis-t-[23]") {
    gt <- plot_to_gtable(g)
    inds <- grep(pattern,gt$layout$name)
    n <- length(inds)
    if (length(width)<n) width <- rep(width,length.out=n)
    cols <- gt$layout$l[inds] ## assume 1 col per element
    gt$widths[cols] <- grid::unit(width,rep(unit,n))
    return(ggdraw(gt))
}

pp <- preds %>% switch_epoch()

if (ci_type=="pointrange") {
    ## modify epoch-summary data to add to outbreak-level data
    levels(pp$epoch) <-  c("early","late")
    ## FIXME: fragile, depends on order
    ## was: paste(levels(pp$epoch),"\n(sum)")
    pp <- (pp
        ## growthrates now exponentiated upstream, in test.Rx
        ##    %>% mutate_at(vars(starts_with("growthrate")),exp)
        ## epoch now == (late, late, early, late)
        ## rather than (14th c., 16th-17th c.)
        %>% mutate(outbreak.year=as.character(epoch))
    )
    ss_majors <- (pp
        %>% mutate(epoch=as.character(epoch))
        %>% bind_rows(ss_majors)
        %>% mutate(epoch=factor(epoch,
                    ## FIXME: fragile, depends on values assigned above
                    levels=c("14th c.",
                             "early",
                             "late",
                             "16th-17th c.")),
                   is_sum=factor(!grepl("c\\.",epoch))
                   )
    )
} else {
    ## these need to be present
    pp$is_sum <- TRUE
    ss_majors$is_sum <- TRUE
}



## for modified axes ...
## inverse r-to-R0 transformation: compute r from R0
## FUN clashes with vapply argument; call it IFUN instead
invfun <- function(target,IFUN=R0,interval=c(-100,1000)) {
    if (length(target)>1) return(vapply(target,invfun,IFUN=IFUN,
                                        interval=interval,FUN.VALUE=numeric(1)))
    uniroot(function(x) IFUN(x)-target, interval=interval)$root
}

invR0 <- invfun
invfinalsize <- function(target) invfun(target,
                                        IFUN=function(x) finalsize(R0(x)))

## quick test
stopifnot(all.equal(1.1,invR0(R0(1.1)),tolerance=1e-4))
stopifnot(all.equal(1.1,invfinalsize(finalsize(R0(1.1))),tolerance=1e-4))

hack_labels <- function(value){
    r <- ifelse(grepl("sum",value),
                sprintf("\\tiny %s",value), value)
    print(r)
    return(r)
}

## plot individual epidemic fits & CIs
gg0 <- (ggplot(ss_majors,aes(outbreak.year,growthrate.value,
                             colour=source,shape=source))
    + facet_grid(.~epoch,scales="free",space="free")
    ## labeller=labeller(epoch=hack_labels))
    + geom_pointrange(position=position_dodge(width=0.6),
                      aes(ymin=growthrate.lower,ymax=growthrate.upper,
                          size=is_sum))
    + scale_colour_manual(values=colvec1)
    + scale_shape_manual(values=pchvec1)
    + labs(x="")
    ## FIXME: adding theme_bw() screws up axes ??
    ## + theme_bw()  ## add theme explicitly
    + theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            panel.grid.major.y=element_line(colour="gray"),
            panel.border=element_rect(colour="black",linetype=1,size=1),
            strip.background=element_rect(fill="gray",colour="black",
                                          linetype=1,size=1),
            ## expand top margin slightly (room for y-axis labels at top)
            ## gets lost/not effective after fancy axis modification?
            plot.margin=margin(t = 9, r = 5.5, b = 2, l = 5.5, unit = "pt")
            )
)

## add results of epoch model; predictions and confidence bands
if (ci_type=="lines") {
    gg1 <- (gg0
        + geom_hline(data=pp,
                     aes(yintercept=exp(growthrate.value),
                         colour=source),
                     linetype=1,
                     size=1.5)
        ## FIXME: melt into a single column and do this with one geom?
        + geom_hline(data=pp,
                     aes(yintercept=exp(growthrate.lower),
                         colour=source),
                     linetype=2)
        + geom_hline(data=pp,
                     aes(yintercept=exp(growthrate.upper),
                         colour=source),
                     linetype=2)
        + scale_size_manual(guide=FALSE,values=size)
)
} else if (ci_type=="bands") {
    gg1 <- (gg0
        + geom_hline(data=pp,
                 aes(yintercept=exp(growthrate.value),
                     colour=source),
                 linetype=2)
    + geom_rect(data=pp,aes(ymin=exp(growthrate.lower),
                            ymax=exp(growthrate.upper),
                            fill=source),
                xmin=-Inf,xmax=Inf,
                alpha=0.3,colour=NA)
        + scale_fill_manual(values=colvec1)
        + scale_size_manual(values=size)
)
} else {
    ## pointrange: gg0 already includes epoch-level info, don't need
    ## to add it
    gg1 <- gg0 + scale_size_manual(guide=FALSE,values=c(size,2*size))
    ## trying to adjust widths: completely scrambles table!
    ##   do I need to keep intermediate as gtable ???
    ## gg1 <- hack_widths(gg0,3)
}

## label for single-axis layout (somewhat obsolete)
R0lab <- sprintf("intrinsic reproductive number (${\\mathcal R}_0$)\n(assuming pneumonic plague%s)",
                 ifelse(incl_ref," [4]",""))

## axis theme for putting y-axis label at top of the plot
## somewhat obsolete since we are now suppressing y-axis labels and re-adding them with draw_label()
## hjust is useless since justification is done only within the individual grid-panel?
## ax_theme <- theme(axis.title.y.left=element_text(angle=0,vjust=1.05,hjust=-2),
##                 legend.position="none")

ax_theme0 <- theme(axis.title.y.left=element_blank())
ax_theme1 <- theme(axis.title.y.left=element_blank(),
                   legend.position="none")

rlims <- c(2.8,55) ## a little narrower than default
## base plot (growth-rate axis)
gg2A <- (gg1
    + scale_y_log10(breaks=c(5,10,20,50),
                    name="",
                    limits=rlims)
    ## hand-adjusted to fall within panel
    + theme(legend.position=c(0.08,0.8),
            legend.background=element_rect(fill="white",color="gray"))
    + ax_theme0
)


## plot version with R0-axis
R0_breaks <- c(1.1,1.2,1.5,2)
gg2B <- (ggplot(ss_majors,aes(outbreak.year,growthrate.value))
    + scale_y_log10(breaks=invR0(R0_breaks),
                    labels=R0_breaks,
                    limits=rlims,
                    name = "") ## ${\\mathcal R}_0$")+
    + geom_blank(aes(NA,growthrate.value))
    + geom_vline(aes(xintercept=-Inf))
    + labs(x="")
    + scale_x_discrete(breaks=NULL)
    + ax_theme1
)


## gg2B <- (gg1
##     + scale_y_log10(breaks=invR0(R0_breaks),
##                     labels=R0_breaks,
##                     limits=rlims,
##                     name = "") ## ${\\mathcal R}_0$")+
##     + ax_theme1
## )

## plot version with Z-axis
finalsize_breaks <- c(0.1,0.2,0.4,0.6,0.8)
gg2C <- (ggplot(ss_majors,aes(outbreak.year,growthrate.value))
    + scale_y_log10(breaks=invfinalsize(finalsize_breaks),
                    labels=finalsize_breaks,
                    limits=rlims,
                    name = "") ## ${\\mathcal R}_0$")+
    + geom_blank(aes(NA,growthrate.value))
    + geom_vline(aes(xintercept=-Inf))
    + labs(x="")
    + scale_x_discrete(breaks=NULL)
    + ax_theme1
)

## gg2C <- (gg1
##     + scale_y_log10(breaks=invfinalsize(finalsize_breaks),
##                     labels=finalsize_breaks,
##                     limits=rlims,
##                     name = "")
##     + ax_theme1
## )

## testing
## plot_grid(gg2A,gg2B,gg2C,nrow=1)

## ADJUST AXIS/LABEL SPACING
## spacing between (r/R0) and (R0/Z) axes, respectively; units=cm
axis_padding <- c(0.18,0.25)  
## hand-adjusted x positions of axis titles
## values are in units of "proportion of plot", measured from left to right
yaxpos <- c("\\scalebox{0.8}{$Z_{\\mbox{\\tiny P}}$}"=0.02,
            "\\scalebox{0.8}{${\\mathcal R}_{0,{\\mbox{\\tiny P}}}$}"=0.065,
            "$r$"=0.11)

names(yaxpos) <- paste(names(yaxpos),c("\n ","\n ","\n \\tiny (/yr)"))

## combine all three sets of axes ...
fig <- (gg2A
    %>% combine_axes(gg2B,
                     add_pos="l",
                     return_gtable=TRUE,
                     pattern="(axis-l|ylab-l|panel)",
                     pad_inner=axis_padding[1])
    %>% combine_axes(gg2C,add_pos="l",
                     pattern="(axis-l|ylab-l|panel)",
                     pad_inner=axis_padding[2])
)

##    + geom_dl(method="top.bumpup",aes(label=source,y=growthrate.upper+1))

## SQUASH: panel.spacing.x=grid::unit(0,"lines")

xtheme <- theme(plot.margin=margin(t = 2, r = 1, b = 1, l = 1, unit = "cm"))
## add axis titles directly atop axes

## add another layer ... ?
## fig <- ggdraw(fig)

fig$coordinates$clip <- "off"  ## ????

for (i in seq_along(yaxpos)) {
    fig <- fig +
        draw_label(names(yaxpos)[i],
                   x= yaxpos[i],
                   y = 0.965)
}

## JM's setup/close stuff confused by ".tex" output extension;
## don't use 'plot.setup(output,width=width,height=height)' any more

if (!interactive()) tikz(file=output, width=width, height=height,
                         standAlone=TRUE)

## adding big margin at this point doesn't help.
suppressWarnings(print(fig
                       +theme(plot.margin=margin(t = 9, r = 5, b = 2, l = 5.5, unit = "pt"))))

if (!interactive()) dev.off()
