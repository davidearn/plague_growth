## show bills, wills and parish registered so we can see the major
## epidemics are evident simultaneously no matter what source we
## examine
args <- commandArgs(trailingOnly=TRUE)

texname <- paste0(rtargetname, ".tex")
defaults = list(file="",standAlone=TRUE,width=6.5,height=3)

file = if (length(args) < 1) texname else args[1]
standAlone = if (length(args) < 2) defaults$standAlone else as.logical(args[2])
width = if (length(args) < 3) defaults$width else as.numeric(args[3])
height = if (length(args) < 4) defaults$height else as.numeric(args[4])

cat(width,height,"\n")

library(epigrowthfit)
library(dplyr)
library(lubridate)
library(tikzDevice)
library(ggplot2)
theme_set(theme_bw())

tex_inputs <- Sys.getenv("TEXINPUTS")
Sys.setenv(TEXINPUTS = paste(getwd(), tex_inputs, sep = .Platform$path.sep))
tikz("crown.tex")

aggWeeks <- 4
aggUnit <- paste(aggWeeks,"weeks")
myylab <- if (aggUnit=="1 week") "Weekly counts" else paste(aggWeeks, "-weekly counts")

h_wills <- aggregate_wills(husting_wills_individual,
                         include_all=TRUE, aggregation=aggUnit)
c_wills <- aggregate_wills(canterbury_wills_individual,
                           include_all=TRUE, aggregation=aggUnit)

all_wills <- bind_rows(h_wills,c_wills) %>%
    select(date,time,nwills,outbreak.year) %>%
    rename(deaths="nwills")

LBoM <- london_bills %>%
    select(time,plague.deaths) %>%
    rename(deaths="plague.deaths")

parish <- london_parish_all %>%
    select(time,total) %>%
    rename(deaths="total")

if (aggWeeks > 1) {
  ## we need to aggregate the LBoM and parish time series further
  parish <- with(parish,aggsum(time=time,deaths=deaths,period=aggUnit))
  LBoM <- with(LBoM,aggsum(time=time,deaths=deaths,period=aggUnit))
}

dd <- bind_rows(list(LBoM=LBoM,wills=all_wills,
                     parish=parish),.id="source")  %>%
    filter((1340<time & time<1380) | (1540<time & time<1680)) %>%
    ## slightly different version of add_epoch (time-based)
    mutate(epoch = ifelse(time < 1400, "14th c.", "16th-17th c."),
           source=factor(source,levels=names(colvec1)))

           

late_majors_yrs <- (london_bills
    %>% select(outbreak.year,severity)
    %>% filter(severity=="major")
    %>% unique()
    %>% pull(outbreak.year)
)

oy_dat <- (dd
    %>% filter(!is.na(outbreak.year))
    %>% group_by(outbreak.year)
    %>% summarise(mintime=min(time),
                  maxtime=max(time))
    %>% filter(outbreak.year %in% late_majors_yrs |
               outbreak.year <1400)
    %>% add_epoch()
)

if (!interactive()) tikz(file, width=width, height=height,
                         standAlone=standAlone)

## need source column not to be called "source"
## (messes with NSE below)
## "sweating sickness" divided into two labels, stacked,
## because there's no easy way to change line leading in R
event_tab <- read.table(header=TRUE,
                            text="
begin      end         dsource  nx fac  label size
1551-06-01 1551-06-01  parish   0 7.7 sweating 1.5
1551-01-01 1551-12-31  parish   0 5.5  sickness 1.5
1557-01-01 1559-12-31  parish   0 3  flu                2
1649-01-30 1660-06-29  parish   1 2  Interregnum  2
1558-11-17 1558-11-17  parish   0 2.2    \\includegraphics[width=15pt]{crown.png} 2",
colClasses=c("Date","Date","character","numeric","numeric","character",
             "numeric"))

event_tab$label <- gsub("_","\n",event_tab$label)
## nx = x-nudge (years, label only); fac = (multiplicative) y-nudge

time_eps <- 0.02  ## imperfect match between dates and parish time points
event_tab2 <- (event_tab
    %>% mutate(n=1:nrow(event_tab))
    %>% group_by(n)
    %>% mutate(begin=lubridate::decimal_date(begin),
               end=lubridate::decimal_date(end),
               lab_x=seq(begin,end,length.out=3)[2]+nx, ## get midpoint
               epoch="16th-17th c.",
               y=fac*mean(pull(filter(dd,source==dsource,
                                  time>=begin-time_eps,
                                  time<=end+time_eps),"deaths")))
)                   

colvec1 <- colvec1[c("parish","wills","LBoM")]  ## change order
brkvec <- c("parish","wills","LBoM")
plot_all <- (ggplot(dd,aes(x=time,y=deaths,colour=source))
    + geom_rect(data=oy_dat,
              aes(group=outbreak.year,
                  xmin=mintime,xmax=maxtime),
              ymin=-Inf,ymax=Inf,
              inherit.aes=FALSE,
              fill="goldenrod",
              alpha=0.5,colour=NA)
    + geom_line(aes(linetype=source),size=1)
    + geom_point(aes(shape=source),size=0.5)
    + scale_shape_manual(values=c(parish=NA,wills=NA,LBoM=15),breaks=brkvec)
    + scale_linetype_manual(values=c(parish=1,wills=1,LBoM=NA),breaks=brkvec)
    + scale_colour_manual(values=colvec1,breaks=brkvec)
    + scale_y_log10(breaks=c(1,10,100,1000,10000))
    + labs(x="date",y="wills or deaths / 4 weeks")
    + facet_grid(.~epoch,scale="free",space="free")
    + geom_segment(data=event_tab2,aes(x=begin,xend=end,y=y,yend=y),colour="black")
    + geom_text(data=event_tab2,aes(x=lab_x,y=y,label=label,size=size),
                colour="black",nudge_y=0.1)
    + scale_size(range=c(1.5,2),guide="none")
    + theme(legend.position=c(0.3,0.8),
            legend.background=element_blank(),
            legend.key.size=grid::unit(0.75,"lines"), ## squish key a little bit
            panel.grid.minor.y=element_blank())
)

print(plot_all)
                      
if (!interactive()) dev.off()
