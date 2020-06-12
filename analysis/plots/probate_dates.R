library(epigrowthfit)
library(dplyr)
source("../fits/epidemics.R")
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)

args = commandArgs(trailingOnly=TRUE)
file = pdfname
file = paste0(rtargetname, ".tex")

## FIXME: this should probably be independent
if (require(willsr)) {
    ## probate dates and times in husting_wills_individual are not clean/complete
    ww <- (willsr::wills
        %>% select(ProbateDate)
        %>% mutate(time=lubridate::decimal_date(ProbateDate))
    )
    saveRDS(ww, file = "../../data/probate.rds")
} else {
    ww <- readRDS("../../data/probate.rds")
}

## get info on wills-written dates from epigrowthfit
## (some repetition from analysis/plots/timeseries.R here)

L1 <- load('../fits/.epidemics.RData')
L2 <- load('../fits/.comb.RData')  ## incl dd_majors2

## utility: extract time range of each outbreak year
get_rng <- (. %>% group_by(outbreak.year)
    %>% select(date)
    %>% summarise(min=min(date),max=max(date))
    %>% ungroup()
)

dd_majors3 <- (dd_majors2
    %>% filter(time<1400)
    %>% select(-c(source,time))
    %>% mutate_at("outbreak.year",as.character)
    %>% rename(count="deaths")
)

ed_early <- (filter(epidemic_defs,end<1400)
    %>% mutate_at("outbreak.year",as.character)
    %>% full_join(get_rng(dd_majors3))
)

probate_extend <- 1 ## extra years


## for a given outbreak year (row of the epidemic definitions table),
##  extract the relevant probate wills and aggregate them weekly,
##  starting from the first day of the *written wills* aggregation
##  previously constructed ...
get_epid_counts <- function(ee) {
    ## take values up to the next probate *after* the extension period
    next_probate <- head(ww$time[ww$time>ee[["end"]]]+probate_extend,1)
    ww0 <- ww[get_range(c(ee[["start"]],next_probate),
                        data=ww, maxlen=Inf),]
    get_date_count(ww0,
                   unit="1 week",
                   date_col="ProbateDate",
                   count_col="probate_count",
                   start_date=ee[["min"]])
}

ww2 <- (plyr::adply(ed_early,1, get_epid_counts)
    %>% rename(date="ProbateDate",count=probate_count)
    %>% select(outbreak.year,date,count)
    %>% ungroup()
    %>% mutate(outbreak.year=as.character(outbreak.year))
)

## check
## ww2 %>% get_rng
## dd_majors3 %>% get_rng

stopifnot(all.equal(ww2 %>% get_rng %>% pull("min"),
                    dd_majors3 %>% get_rng %>% pull("min")))

dd_comb <- (bind_rows(list(written=dd_majors3,probate=ww2),.id="type")
    %>% mutate(time=lubridate::decimal_date(date),
               ## modify labels
               type=factor(type,levels=c("probate","written"),
                           labels=c("probated","written")))
    %>% arrange(date)
)

figure <- (ggplot(dd_comb,aes(date,count,colour=type,shape=type,linetype=type))
    + geom_point()
    ## + geom_smooth(method="gam",
    ## method.args=list(family=quasipoisson),
    ## formula=y~s(x,k=5))
    + geom_smooth(span=0.5,method="loess")
    + scale_y_continuous(
          trans="log1p"
                       , limits=c(0,80)
                       , expand=c(0.02,0)
                       , oob=scales::squish
                         )
    + facet_wrap(~outbreak.year,scale="free_x")
    + scale_colour_brewer(palette="Set2")
    + labs(y="count\n(log(1+x) scale)")
    + scale_x_date(labels=function(x) format(x,"%Y"))
)

plot.setup(file, width=8, height=6)
print(figure)
plot.close(file)
