args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) paste0(rtargetname, ".tex") else args[1]

## memisc::mtable
## texreg
## huxtable
library(huxtable)
library(glmmTMB)
library(broom.mixed)

cc <- c("Intercept (14th-c. wills log growth rate)"="(Intercept)",
        "epoch (late vs. early)"="epochlate",
        "source (parish vs. wills)"="sourceparish",
        "source (LBoM vs. wills)"="sourceLBoM")

h0 <- huxreg(" "=f0, # give model blank name so we don't get '(1)'
             tidy_args=list(effects="fixed",conf.int=TRUE),
             coefs=cc,
             error_format = '({conf.low} -- {conf.high})',
             stars=NULL, ## no sig. stars
             error_pos="right",
             statistics="nobs" # don't include logLik and AIC
             )
names(h0) <- c("","estimate","95% CI")
h1 <- add_colnames(h0)
cat(to_latex(h1,tabular_only=TRUE),
    file=file,append=FALSE)



