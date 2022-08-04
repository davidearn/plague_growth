## combine data and fits
output = ".comb.RData"

library(dplyr)
library(tidyr)
library(methods) # needed for as(); needs to go *before* epigrowthfit!
library(epigrowthfitPNAS)

## testing GOF
## summary(fitList[[2]][[1]])
## trace("gof",sig="epigrowthfit",browser)
## summary(fitList[[2]][[1]],gof_agg="7 days")

ss_majors <-(
    summary.fitList(fitList,level=2)
    %>% drop_bad
    ## convert source labels to match previous fits (lump wills together)
    %>% mutate(source= mod_source(source))
    %>% select(source,outbreak.year,starts_with("growthrate"),gof)
    %>% add_epoch
)
## summary(fitList[["London bills"]][["1593"]])
## data (time series of wills/plague deaths)
dd_majors <- (summary.fitList(fitList,level=2,type="deaths")
    %>% drop_bad
    %>%  mutate(source=mod_source(source))
)

## model predictions
pp_majors <- (summary.fitList(fitList,level=2,type="pred", confint_pred=FALSE)
    %>% drop_bad
    %>%  mutate(source=mod_source(source))
)

if (FALSE) {
    ## issues with 1593? which source?
    ## testing
    library(ggplot2)
    ggplot(pp_majors,
           ## subset(pp_majors,as.numeric(outbreak.year)<1400),
           aes(time,deaths,ymin=lwr,ymax=upr))+
        geom_line(aes(colour=source))+
        geom_ribbon(aes(fill=source),alpha=0.3)+
        facet_wrap(~outbreak.year,scale="free")+
        scale_y_log10()
    predict(fitList[["Canterbury wills"]][["1625"]],confint=TRUE)
    pp1 <- filter(pp_majors ,outbreak.year=="1")
    debug(bbmle:::pop_pred_samp)
    debug(emdbook::dmvnorm)
    debug(MASS::mvrnorm)
    trace("predict",sig="epigrowthfit",browser)

}

pp_wills_7day <- pp_majors %>%
    filter(source=="wills") %>%
    ## keep source in group_by to retain it in results
    group_by(outbreak.year,source) %>%
    do(aggsum(time=.$time,deaths=.$deaths,period="7 days")) %>%
    ungroup

dd_wills_7day <- dd_majors %>%
    filter(source=="wills") %>%
    group_by(outbreak.year,source) %>%
    do(aggsum(time=.$time,deaths=.$deaths,period="7 days")) %>%
    ungroup

## compute GOF
## FIXME: need to aggregate to 7 days differently to get matching time periods
(pp_wills_7day %>%
 rename(pred=deaths) %>%
 select(outbreak.year,date,pred) %>%
 full_join(dd_wills_7day,by=c("outbreak.year","date"))
) %>% head()

## replace wills with 7-day aggregated wills
dd_majors2 <- (dd_majors
    %>% filter(source != "wills")
    %>% bind_rows(dd_wills_7day))
pp_majors2 <- (pp_majors
    %>% filter(source != "wills")
    %>% bind_rows(pp_wills_7day))

## DON'T save.image()! saves output variable, corrupts downstream ...
save(list=ls(pattern="^(ss|pp|dd)_"),file=output)

## rdnosave
