library(methods)
library(dplyr)

library(epigrowthfitPNAS)
library(glmmTMB)
ss <- (summary.fitList(fitList,level=2)
    ## log-transform ... and rename ...
    %>% mutate_at(vars(starts_with("growthrate")),log)
    %>% dplyr::rename(log.r=growthrate.value,
                      log.r.min=growthrate.lower,
                      log.r.max=growthrate.upper)
    %>% select(outbreak.year,source,starts_with("log.r"))
    %>% add_epoch_EL()
    %>% mutate(sdvals=(log.r.max-log.r.min)/diff(qnorm(c(0.025,0.975))))
    %>% drop_bad ## no Canterbury/1563,1593; LBoM/1593
    ## lump canterbury/husting (*after* drop_bad!)
    %>% mutate(source=mod_source(source))
)

f0 <- fitfun(ss,method="glmmTMB")
s0 <- p_sumfun(f0)
permtab <- allperms(ss)
obsval <- s0$estimate  ## observed log-difference in growthrates
hist(permtab$estimate,col="gray",breaks="FD")
## pvalue <- mean(abs(obsval)<=abs(permtab$estimate))
perm_count1 <- sum(obsval<=permtab$estimate) ## 1-sided, directional
perm_pvalue1 <- mean(obsval<=permtab$estimate) ## 1-sided, directional
perm_count <- 2*sum(obsval<=permtab$estimate) ## 1-sided, directional
perm_pvalue <- 2*perm_pvalue1

## predictions (no re.form for glmmTMB)

pframe <- (with(ss,expand.grid(epoch=unique(epoch),
                     source=unique(source),
                     outbreak.year=NA,
                     sdvals=1)))
pred0 <- predict(f0,newdata=pframe,se.fit=TRUE)

preds <- (pframe
    %>%   mutate(growthrate.value=pred0$fit,
                 growthrate.lower=pred0$fit-1.96*pred0$se.fit,
                 growthrate.upper=pred0$fit+1.96*pred0$se.fit)
    %>% mutate_at(vars(starts_with("growthrate")),exp)
    %>% mutate(doublingtime.value=doublingTime(growthrate.value),
               ## NB lower/upper switched here (inverted scale)
               doublingtime.lower=doublingTime(growthrate.upper),
               doublingtime.upper=doublingTime(growthrate.lower),
               R0.value=R0(growthrate.value),
               R0.lower=R0(growthrate.lower),
               R0.upper=R0(growthrate.upper),
               finalsize.value=finalsize(R0.value),
               finalsize.lower=finalsize(R0.lower),
               finalsize.upper=finalsize(R0.upper))
    %>% filter(!(epoch=="early" & source!="wills"))
)

## save("permtab","preds","pvalue","ss","f0",file="data/test.RData")

## alternative tests: e.g. Wilcox/Mann-Whitney

## all sources (indiscriminate)
with(ss,wilcox.test(log.r[epoch=="early"],log.r[epoch=="late"]))

## wills only
subset(ss,source=="wills",select=c(epoch,outbreak.year,log.r))
## only 4 early, 3 late: 1 crossover
with(subset(ss,source=="wills"),
     wilcox.test(log.r[epoch=="early"],log.r[epoch=="late"]))
