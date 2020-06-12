library(methods)
library(emdbook)
library(epigrowthfit)
library(dplyr)

## default is warn=0.  warn=1 gives 'real time' warnings rather than
##  saving warnings and printing ">50 warnings occurred ..."
## good for debugging, but right now we have lots of hard-to-suppress
##  warnings emanating from lbeta ("underflow occurred in 'lgammacor'")
## options(warn=1)

# pick the subset of data corresponding to an epidemic
#' @param epidemic the epidemic defined by set.epidemic
#' @param data a dataframe with columns time and plague.deaths
pick.epidemic <- function(epidemic, data) {
  subset(data, outbreak.year == epidemic)
}

#' aggregate a time series of deaths into monthly deaths
#' @param data the epidemic defined by set.epidemic
aggregate.monthly <- function(data) {
  if (nrow(data) == 1) return(data)
  stack = data[1,]
  push <- function(row) {
    if (row$time - stack$time > 27/366) { ## FIXME: what does 27/366 signify?
      summed = stack
      stack = row
      return(list(stack=stack, pop=summed))
    } 
    stack$plague.deaths = stack$plague.deaths + row$plague.deaths
    list(stack=stack, pop=data.frame())
  }
  aggregated = data.frame()
  # only aggregate to monthly if we have more than 1 row of data
  for (i in 2:nrow(data)) {
    out = push(data[i, ])
    stack = out$stack
    aggregated = rbind(aggregated, out$pop)
  }
  rbind(aggregated, stack)
}

extract_baseline <- function(data, yr, unit=c("year","step"),
                             debug=FALSE) {
    unit <- match.arg(unit)
    ## cases per reporting step ...
    bb <- attr(data,"baseline")[as.character(yr)]
    if (debug) cat("baseline (1): ",bb,"\n")
    if (unit=="year") {
        ## time step ...
        dt <- diff(data$time)[1]
        if (debug) cat("dt (1): ",dt,"\n")
        ## -> cases/year
        bb <- bb/dt
        if (debug) cat("baseline (2): ",bb,"\n")
    }
    return(bb)
}

#' initialize fitEpidemic
#' @param data the plague data, with columns time and plague.deaths and epidemics
#' @param first optional named list of hard-coded parameters for window start
#' (as index of position within outbreak year); if specified as a single value,
#' use for all outbreak years
#' @param first_level optional named list of hard-coded parameters for window starting level
#' (as proportion of peak)
#' @examples
#' ff <- fit.epidemics(subset(london_bills,severity=="major"))
fit.epidemics <- function(data, first=NULL, first_level=NULL,
                          first_time = NULL,
                          fixed_baseline=FALSE,
                          max_theta=1e4,
                          debug = FALSE,
                          theta0_list=NULL, ...) {
    fits <- list()
    for (e in unique(data$outbreak.year)) {
        cat("processing", e, "\n")
        epidemic <- subset(data, outbreak.year == e)
        e <- as.character(e)
        if (!is.null(theta0_list)) {
            theta0 <- theta0_list[[as.character(e)]]
        } else theta0 <- NULL
        
        ## no longer setting model="richards", loglik = "nbinom";
        ##   use options() to set
        argList <- list(data=epidemic)
        argList <- c(argList,list(...), list(theta0=theta0))

        ## check for hard-coded window information, add it to argument list if found
        if (length(first)==1) {
            argList$first <- first
        } else {
            if (e %in% names(first)) {
                argList$first <- first[[e]]
            } else if (e %in% names(first_level)) {
                argList$first_level <- first_level[[e]]
            } else if (e %in% names(first_time)) {
                argList$first_time <- first_time[[e]]
            }
        }
        if (fixed_baseline) {
            bb <- extract_baseline(data,e,debug=debug)
            argList$fixed <- list(b=unname(log(bb)))
        }
        if (debug) {
            cat("argList:\n")
            print(argList[!names(argList)=="data"])
        }
        fit <- try(do.call(epigrowthfit,argList))
        if (inherits(fit,"try-error")) {
            break  ## skip! don't break summaries etc by including NA/NULL etc.
        }
        if (is.finite(max_theta)) {
            th <- coef(fit)["ll.k"]
            if (!is.na(th) && th>max_theta) {
                argList$distrib <- "poisson"
                message("refitting with Poisson")
                fit <- do.call(epigrowthfit,argList)
            }
        }
        fits[[e]] <- fit
    } ## loop over years
    class(fits) <- "fitEpidemics"
    return(fits)
}

summary.fitEpidemics <- function(object, ...) {
    ss <- lapply(object,summary)
    dd <- lapply(ss,as,Class="data.frame")
    ## use bind_rows() to handle the case where different models
    ##  are fitted to different elements (because of small windows etc.)
    ##  and hence parameter sets differ ...
    dplyr::bind_rows(dd,.id="outbreak.year")  
}

#' sample the plague deaths by a report ratio
#' @param data the plague mortality data, with a column $plague.deaths
#' @param report.ratio the sample ratio
sample.data <- function(data, report.ratio) {
  pos <- which(data$plague.deaths > 0)
  while (TRUE) {
    deaths <- round(data$plague.deaths)
    d <- deaths[pos]
    deaths[pos] <- rbinom(d, d, report.ratio)
    zero = c(0, which(deaths == 0), length(deaths) + 1)
    seg <- diff(zero)
    if (max(seg) > 3) break
  }
  data$plague.deaths = deaths
  data
}

#' sampledEpidemic initializer
sample.epidemic <- function(data, sample.ratio, runs) {
    epidemic <- as.character(data$outbreak.year[1])
    sample.ratio <- min(sample.ratio, 1)
    if (sample.ratio == 1) {
        runs <- 1
        sampled <- data
    } else {
        d <- list()
        for (i in 1:runs) {
            d[[i]] <- data.frame(
                outbreak.year=sprintf("%s (%i)", epidemic, i),
                sample.data(data, sample.ratio)
            )
        }
        sampled <- do.call(rbind,d)
    }

    attr(sampled, "sample.ratio") <- sample.ratio
    attr(sampled, "runs") <- runs
    class(sampled) <- "sampledEpidemic"
    return(sampled)
}

## previous fits from factorial comparison of fits on Hustings wills
## recode/lump sources from current fits
mod_source <- function(x) {
    x <- as.character(x)
    ## lump Husting and Canterbury wills together
    x <- ifelse(grepl("wills",x),"wills",x)
    factor(x,
           levels=c("wills",
                    "London parish",
                    "London bills"),
           labels=c("wills","parish","LBoM"))
}

## data sets considered too bad to bother with
drop_bad <- . %>%
    dplyr::filter(!(source %in% c("Canterbury wills","wills") &
             outbreak.year %in% c("1563","1593")),
           !(source %in% c("London bills","LBoM") &
             outbreak.year=="1593"))

add_epoch <- . %>%
    mutate(epoch=ifelse(outbreak.year<1500,"14th c.","16th-17th c."))
## same, but with early/late labels
add_epoch_EL <- . %>%
    mutate(epoch=ifelse(outbreak.year<1500,"early","late"))

switch_epoch <- . %>%
    mutate(epoch=factor(epoch,levels=c("early","late"),
                        labels=c("14th c.","16th-17th c.")))


#' @param ep endpoints (in decimal time)
#' @param maxlen max window length (in weeks; avoids overlaps in cases where the duration of "outbreak year" is 2 years)
get_range <- function(ep,maxlen=round(52*1.5),
                      data=parish,timecol="time") {
    s <- seq(which.min(abs(data[[timecol]]-ep[1])),
             which.min(abs(data[[timecol]]-ep[2])))
    if (length(s)>maxlen) {
        s <- s[1:maxlen]
    }
    return(s)
}

##' extract convergence info from fitList
##' (should wrap this into summary.fitList
get_conv <- function(x) {
    cc <- map_dbl(x,function(y) y@mle2@details$conv)
    ll <- map_dbl(x, ~(-1)*c(logLik(.)))
    mm <- map_chr(x,function(y) {
        r <- y@mle2@details$message
        if (is.null(r)) "" else r
    })
    tibble(outbreak.year=names(cc), conv=cc, msg=mm, nll=ll)
}

nloptControl <- function(algorithm = "NLOPT_LN_BOBYQA",
                         xtol_abs = 1e-06, ftol_abs = 1e-06, 
                         maxeval = 1e+05, ...) {
    lme4:::namedList(algorithm, xtol_abs, ftol_abs, maxeval, ...)
}
    
## my version of nloptwrap: less obscure arguments than the lme4 version,
##  intercept optim-specific arguments
nloptwrap <- function (par, fn, lower=NULL, upper=NULL, gr=NULL,
                       control = nloptControl(), ...) 
{
    if (length(control)==0) control <- nloptControl() ## hack
    ## uses par from local environment
    check_bound <- function(bound,nm) {
        if (length(bound)==0) {
            bound <- if (nm=="lower") -Inf else Inf
        }
        if (length(bound)==1) {
            bound <- rep(bound,length(par))
        }
        if (length(bound)!=length(par)) {
            stop(sprintf("length mismatch between %s (%d) and par (%d)",
                         nm,length(bound),length(par)))
        }
        return(bound)
    }
    L <- list(...)
    ## remove optim-specific arguments
    L$method <- L$hessian <- L$strict <- NULL
    argList <- c(L,
                 list(x0 = par, eval_f = fn, eval_grad_f = gr,
                      lb = check_bound(lower,"lower"), ub = check_bound(upper,"upper"),
                      opts = control))
    res <- do.call(nloptr,argList)
    with(res, list(par = solution, value = objective, counts = iterations, 
                   convergence = if (status > 0) 0 else status, message = message))
}
