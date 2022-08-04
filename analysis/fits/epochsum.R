library(methods)

library(glmmTMB)
if (packageVersion("glmmTMB")<"0.2.2.0") {
    stop("need latest version of glmmTMB: ",
         "devtools::install_github('glmmTMB/glmmTMB/glmmTMB')")
}
library(epigrowthfitPNAS)
library(emmeans)
library(multcomp)

## predicted values for wills
dd <- data.frame(epoch=c("early","late"),source="wills",
                 outbreak.year=NA,sdvals=NA)
logrvals <- predict(f0,newdata=dd,se.fit=TRUE)

e1 <- emmeans(f0, ~epoch)
##
## est df
ddf <- summary(e1)$df[1]
ciwid <- qt(0.975,ddf) ## 2.22
logrtab <- with(logrvals, rbind(fit, lower=fit - ciwid*se.fit,
                                upper=fit + ciwid*se.fit))
colnames(logrtab) <- c("early","late")

## The following three tables contain est,lower,upper for each
## epoch (early or late), based solely on wills:
rvals <- exp(logrtab)
R0vals <- R0(rvals)
Zvals <- finalsize(R0vals)

## crude estimates based on generation time of 18 days = 0.05 year
R0valsb <- rvals*0.05+1
Zvalsb <- finalsize(R0valsb)

## R0 ests based on all sources
R0all <- R0(exp(as.matrix(summary(e1)[,c("emmean","lower.CL","upper.CL")])))
## match R0vals format
R0all <- t(R0all)
dimnames(R0all) <- dimnames(R0vals)

Zall <- finalsize(R0all)

ci_ur <- suppressWarnings(confint(f0,method="uniroot"))

## profile CIs slightly wider
## cc <- exp(confint(f0))
## v <- setNames(round(cc["cond.epochlate",],1),c("lwr","upr","est"))

## use t-stat CIs; makes 'v' and 'vall' equivalent
csd <- summary(f0$sdr, "fixed")[, "Std. Error"]
csd <- csd[names(csd)=="beta"]
pp <- fixef(f0)[["cond"]]
w <- which(names(pp)=="epochlate")
v <- round(exp(c(est=pp[[w]],
                 lwr=pp[[w]]-ciwid*csd[[w]],
                 upr=pp[[w]]+ciwid*csd[[w]])),1)

## compute difference based on all sources
## this is IDENTICAL to previous, left in for comparison/check
## contrasts are based on difference from mean, so double estimate & SE
##  to compute (late-early) difference
##  use second row of contrasts (late=positive difference)
sc <- summary(contrast(e1))
vall <- with(sc[2,],
             round(exp(c(est=2*estimate,lwr=2*(estimate-ciwid*SE),
                         upr=2*(estimate+ciwid*SE))),1))

stopifnot(all.equal(v,vall))
catt <- function(...,file="modeldefs.tex") {
    cat(...,file=file,append=TRUE)
}

latexout <- function(x,nm,fmt="%1.1f") {
    catt(sprintf("\\newcommand{\\%s}{%s}\n",nm,sprintf(fmt,x)))
}
unlink("modeldefs.tex")

catt("% epoch-level estimates of change in r between early wills & late wills\n")
latexout(v["est"],"rdiffest")
latexout(v["lwr"],"rdifflwr")
latexout(v["upr"],"rdiffupr")

## round to integer because of the context we want it in
catt("% 14th c r estimates\n")
latexout(rvals["fit","early"],"rearlyest","%1.0f")
latexout(rvals["fit","late"],"rlateest","%1.0f")

catt("% 14th c R0 bubonic estimates\n")
latexout(R0valsb["fit","early"],"rzeroearlybest","%1.1f")

catt("% 14th c Z bubonic estimates\n")
latexout(round(100*Zvalsb["fit","early"]),"Zearlybest","%d")

catt("% late R0 bubonic estimates\n")
latexout(R0valsb["fit","late"],"rzerolatebest","%1.1f")
latexout(R0valsb["lower","late"],"rzerolateblwr","%1.1f")
latexout(R0valsb["upper","late"],"rzerolatebupr","%1.1f")

catt("% late Z bubonic estimates\n")
latexout(round(100*Zvalsb["fit","late"]),"Zlatebest","%d")
latexout(round(100*Zvalsb["lower","late"]),"Zlateblwr","%d")
latexout(round(100*Zvalsb["upper","late"]),"Zlatebupr","%d")

catt("% 14th c R0 pneum estimates\n")
latexout(R0vals["fit","early"],"rzeroearlyest","%1.1f")
latexout(R0vals["lower","early"],"rzeroearlylwr","%1.2f")
latexout(R0vals["upper","early"],"rzeroearlyupr","%1.2f")

catt("% 14th c Z estimates\n")
latexout(round(100*Zvals["fit","early"]),"Zearlyest","%d")
latexout(round(100*Zvals["lower","early"],1),"Zearlylwr","%1.1f")
latexout(round(100*Zvals["upper","early"]),"Zearlyupr","%d")

catt("% late R0 estimates\n")
latexout(R0vals["fit","late"],"rzerolateest","%1.1f")
latexout(R0vals["lower","late"],"rzerolatelwr","%1.1f")
latexout(R0vals["upper","late"],"rzerolateupr","%1.1f")

catt("% late Z estimates\n")
latexout(round(100*Zvals["fit","late"]),"Zlateest","%d")
latexout(round(100*Zvals["lower","late"]),"Zlatelwr","%d")
latexout(round(100*Zvals["upper","late"]),"Zlateupr","%d")


catt("% putative rat ratio required to get observed R0 increase\n")
## round before ratio so values match in text! (subtract 1 to get % increase)
latexout(round(100*(round(R0valsb["fit","late"],1)/round(R0valsb["fit","early"],1)-1)),"ratpct",
         "%d")

catt("% number of permutations\n")
latexout(nrow(permtab), "nperm", "%d")

catt("% permutation fraction\n")
## latexout is not quite flexible enough
catt(sprintf("\\newcommand{\\permfrac}{%d/%d}\n",
             perm_count,nrow(permtab)))

catt("% permutation p-value\n")
## latexout is not quite flexible enough
latexout(perm_pvalue, "permpval",  "%1.3f")

cat("% multiplication word 1")
w <- switch(as.character(round(v["est"])),
            "2"="two",
            "3"="three",
            "4"="four",
            "5"="five")
latexout(w, "foldval", "%sfold\\xspace")
