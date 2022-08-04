library(dplyr)
library(purrr)
library(epigrowthfitPNAS)

## extract 'sampling' design from data; which outbreak years
##  do we have which sources for?

get_oy <- function(x,src) {
    if ("severity" %in% names(x)) {
        x <- filter(x,severity=="major")
    }
    x2 <- (x
        %>% select(outbreak.year)
        %>% unique()
        %>% mutate(source=src)) 
    return(x2)
}
plague_years <-
    (map2(list(london_parish,london_bills,husting_wills,canterbury_wills),
         c("parish","LBoM","wills","wills"),
         get_oy)
        %>% bind_rows()
        %>% mutate(epoch=factor(ifelse(outbreak.year<1400,"early","late")))
        %>% arrange(outbreak.year)
    )

true_pars <- list(
    ## fixed effects (intercept; early vs late; wills vs. parish, LBoM)
    beta=c(`(Intercept)`=2.5,epochlate=0.5,sourceparish=0.25,sourceLBoM=0.5), 
    sigma_yr=0.5,  ## among-year sd
    sdvals=c(mu=-1,sd=0.5) ## log-normal params for residual error
)
## reorganize fixed effects into data frame format for later merging/analysis
true_vals <- tibble(term=names(true_pars$beta),
                    true_val=true_pars$beta)


simfun <- function(epochlate=true_pars$beta["epochlate"], ## early/late diff
                   beta=true_pars$beta,         ## all fixed parameters
                   sigma_yr=true_pars$sigma_yr, ## among-year var
                   sdvals=true_pars$sdvals,     ## log-mean/sd of residual sd
                   data=plague_years,
                   checkpoint=FALSE) {
    res <- data
    n <- nrow(data)
    ## substitute specified value ...
    beta["epochlate"] <- epochlate
    ## could use lme4::simulate(), but it doesn't work very well
    ## with variable variances, so ...
    ## construct model matrices for fixed effects and random effects
    X <- model.matrix(~epoch + source,  data=data)
    Z <- model.matrix(~factor(outbreak.year)-1, data=data)
    ## year effects
    b <- rnorm(ncol(Z),mean=0,sd=sigma_yr)
    ## residual SDs
    sdvals <- rlnorm(n,
                     meanlog=sdvals["mu"],
                     sdlog=sdvals["sd"])
    eps <- rnorm(n,mean=0,sd=sdvals)
    ## compute value
    res$log.r <- drop(X %*% beta + Z %*% b + eps)
    res$sdvals <- sdvals ## incorporate in data frame so we can retrieve it
    if (checkpoint) save("res",file=".simfun.RData")
    return(res)
}

##' Fitting function: inverse-variance-weighted mixed model,
##' in glmmTMB or lme4 (use REML=FALSE
##' for consistency with glmmTMB), or collapse data to one point/outbreak year
##' by precision-weighted averaging, then do a weighted t test

fitfun <- function(d,method="lme4") {
    require(lme4)
    require(glmmTMB)
    require(weights)
    fit <- switch(method,
           lme4=lmer(log.r~epoch + source + (1|outbreak.year), data=d,
                weights=1/d$sdvals^2, REML=FALSE),
           glmmTMB=glmmTMB(log.r~epoch + source + (1|outbreak.year),
                           data=d,
                           disp=~1+offset(log(sdvals^2))),
           wtd.t.test={
        dc <- d %>% group_by(outbreak.year) %>% do(collapsefun(.))
        with(dc, {
             x <- log.r[outbreak.year<1400]
             x.w <- prec[outbreak.year<1400]
             y <- log.r[outbreak.year>1400]
             y.w <- prec[outbreak.year>1400]
             w <- wtd.t.test(x,y,x.w,y.w,samedata=FALSE)
             class(w) <- c("wtd.t.test","list")
             w
        })
    })
    attr(fit,"data") <- d  ## store for updating purposes
    return(fit)
}

collapsefun <- function(d) {
    ## pop-level variance (/n rather than /(n-1) so we get var=0
    ##   rather than NA for single values
    pvar <- function(x) sum((x-mean(x))^2)/length(x)
    d2 <- d %>% group_by(outbreak.year) %>%
        mutate(prec=1/sdvals^2) %>%  # inverse-variance
        ## mean = precision-weighted average
        ## variance = mean(variance) + variance(mean) ... ???
        ## should we treat this as a standard error, i.e. divide by sqrt(n)?
        ## have to compute sdvals first (based on original log.r)
        summarise(sdvals=sqrt(mean(sdvals^2)+pvar(log.r)),
                  log.r=sum(log.r*prec)/sum(prec)) %>%
        mutate(prec=1/sdvals^2)  ## recompute precision
    return(d2)
}

p_sumfun <- function(ff,conf.method="Wald",epoch.only=TRUE,
                   p.values=TRUE) {
    require(broom)
    require(broom.mixed)
    ## could filter out intercept, but might as well do it
    ##  at the next step
    if (p.values && !epoch.only && conf.method != "Wald") {
        stop("LRT p-values only implemented for epoch parameters")
    }
    if (epoch.only && conf.method!="Wald") {
        ## replicate some of confint machinery within broom, to
        ##  avoid expensive profiling for all parameters when we
        ##  only want epoch parameter
        tt <- suppressWarnings(tidy(ff,effect="fixed",conf.int=FALSE)) %>%
            filter(term=="epochlate")
        ## TMB needs position, not names
        p <- if (is(ff,"merMod")) "epochlate" else 2
        ci <- as.data.frame(confint(ff,method=conf.method,parm=p))
        tt <- cbind(tt,setNames(ci,c("conf.low","conf.high")))
    } else {
        tt <- suppressWarnings(tidy(ff,effect="fixed",
                                    conf.int=TRUE,conf.method=conf.method))
        if (epoch.only) tt <- tt %>% filter(term=="epochlate")
    }
    if (p.values && conf.method != "Wald") {
        ## LRT p-value
        d <- attr(ff,"data")   ## hack: glmmTMB needs this
        ff_0 <- update(ff,. ~ . - epoch)
        p <- anova(ff_0,ff)["ff","Pr(>Chisq)"]
        tt$p.value <- p
    }
    if (is.null(tt$p.value)) tt$p.value <- NA
    tt <- tt %>% select(term,estimate,p.value,conf.low,conf.high)
    return(tt)
}

tidy.wtd.t.test <- function(object,...) {
    est <- -1*object$additional[["Difference"]]
    se <- object$additional[["Std. Err"]]
    df <- object$coefficient[["df"]]
    pval <- object$coefficient[["p.value"]]
    mult <- qt(0.975,df=df)
    dplyr::data_frame(effect="fixed",
                      group="fixed",
                      term="epochlate",
                      estimate=est,
                      std.error=se,
                      p.value=pval,
                      conf.low=est-mult*se,
                      conf.high=est+mult*se)
}


#' permute epoch within outbreak data
permfun <- function(d,s=plague_years) {
    ## extract info from structure variable and
    ## permute epoch/year assignment
    perms <- (s
        %>% select(outbreak.year,epoch)
        %>% unique()
        %>% mutate(epoch=sample(epoch))
    )
    ## repl
    ret <- (d
        %>% select(-epoch)
        %>% full_join(perms,by="outbreak.year")
    )
    return(ret)
}

## fit all choose(9,4) == 126 permutations
allperms <- function(d,fitmethod="glmmTMB") {
    ## epoch-outbreak.year combinations (without source):
    pp <- unique(subset(d,select=c(epoch,outbreak.year)))
    n_early <- sum(pp$epoch=="early")
    n_tot <- nrow(pp)
    ## generate matrix of all choose(n,k) combinations:
    ## (yields 9 cols by 126 rows)
    cc <- t(combn(n_tot,n_early))
    ## generate a vector of early and late corresponding to a combination:
    gen_ep <- function(x) replace(rep("late",n_tot),x,"early")
    ## generate a new data set like original data passed to function, but
    ## with epochs assigned according to a combination:
    mkdata <- function(x) {
        ppc <- pp
        ppc$epoch <- gen_ep(x)
        ## replace epoch in original data with scrambled epochs:
        newdat <- (d
            %>% select(-epoch)
            %>% full_join(ppc,by="outbreak.year")
        )
        return(newdat)
    }
    ## adply steps through each row of an array, runs a function
    ## (in this case a function that returns a one-line summary data frame),
    ## and returns the results as a combined data frame:
    res <- plyr::adply(cc, 1, function(x) p_sumfun(
                                              fitfun(mkdata(x),
                                                     method=fitmethod)),
                       ## (maybe) print text-based progress bar
                       .progress=if (interactive()) "text" else "none") 
    return(res)
}
