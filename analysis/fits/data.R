library(dplyr)

#' add an epoch column to data
#' @param data a dataframe object with an epidemic column
#' @return a dataframe object
add_epoch <- function(data) {
  m <- mutate(data,epoch=factor(ifelse(epidemic %in% c("1348", "1361", "1375"),
                                       "$<1400$ CE","$>1400$ CE")))
  return(m)
}

## #' the generic method for extracting exponential growth rate
## setGeneric(
##    "growthRate",
##    def = function(fit) {
##      standardGeneric("growthRate")
##    }
## )

#' extract growth rate from an fitEpidemics object
setMethod(
  "growthRate",
  signature="fitEpidemics",
  definition = function(obj) {
    data <- data.frame()
    fit <- obj@epidemics
    for (epidemic in names(fit)) {
        data <- rbind(data, 
                      cbind(as.data.frame(t(fit[[epidemic]]@growthRate)),
                            epidemic = epidemic))
    }
    add_epoch(data)
}
)

#' @export
#' compute goodness of fit for each epigrowthfit object in a fitEpidemics object
setMethod(
  "gof",
  "fitEpidemics",
  definition = function(obj) {
    data <- data.frame()
    fit <- obj@epidemics
    for (epidemic in names(fit)) {
        gofline <- gof(fit[[epidemic]])
        data <- rbind(data, data.frame(value=gofline,lower=NA,upper=NA,epidemic))
    }
    add_epoch(data)
  }
)

#' compute sampled CI
#' @param data a dataframe with columns value, lower and upper
#' @return a vector
sampledCI <- function(data) {
  # if there is a single sample run, do not sample
  if (nrow(data) == 1) return(data[1, c("value", "lower", "upper")])
  # otherwise, sample from a normal distribution centered at value, 
  # with the standard deviation as a quarter of the 95% CI
  value <- mean(data$value)
  rn <- c()
  for (i in 1:nrow(data)) {
    if (any(is.na(data[i,]))) next
    sd <- (data$upper[[i]] - data$lower[[i]]) / 4
    rn <- c(rn, rnorm(1000, mean=data$value[[i]], sd=sd))
  }
  data.frame(value = value,
             lower = quantile(rn, probs=0.025)[[1]],
             upper = quantile(rn, probs=0.975)[[1]])
}

#' extract growth rate from an sampledEpidemic object
setMethod(
  "growthRate",
  "sampledEpidemic",
  definition = function(obj) {
    lambda <- callNextMethod(obj)
    sampledCI(lambda)
  }
)

#' get gof from a sampledEpidemic object (mean of gof across fits)
setMethod(
  "gof",
  "sampledEpidemic",
  definition = function(obj) {
     gof_val <- callNextMethod(obj)
     gof_dd <- data.frame(value=mean(gof_val$value),
                          lower=NA,upper=NA,
                          ## trim labels
                          epoch=gof_val$epoch[1])
     return(gof_dd)
  }
)

multi_samp <- function(obj,FUN) {
    data <- data.frame()
    fit <- obj@epidemics
    for (epidemic in names(fit)) {
        fit.ep <- fit[[epidemic]]
        r <- FUN(fit.ep)
        data <- rbind(data, cbind(r, epidemic = epidemic))
    }
    add_epoch(data)
}
    
#' extract growth rate from sampled fits
setMethod(
  "growthRate",
  "sampled",
  definition = function(obj) {
    multi_samp(obj,growthRate)
}
)

#' extract gof from sampled fits
setMethod(
  "gof",
  "sampled",
  definition = function(obj) {
    multi_samp(obj,gof)
})

#' extract R0 from the fits
setMethod(
  "R0",
  "fitEpidemics",
  definition = function(obj) {
    data <- data.frame()
    obj <- obj@epidemics
    for (epidemic in names(obj)) {
      fit <- obj[[epidemic]]
      data <- rbind(data, 
                    cbind(as.data.frame(t(R0(fit))),
                          epidemic = epidemic))
    }
    add_epoch(data)
  }
)

#' extract doubling time from the fits
setMethod(
  "doublingTime",
  "fitEpidemics",
  definition = function(obj) {
    data <- data.frame()
    obj <- obj@epidemics
    for (epidemic in names(obj)) {
      fit <- obj[[epidemic]]
      data <- rbind(data, 
                    cbind(as.data.frame(t(doublingTime(fit))),
                          epidemic = epidemic))
    }
    add_epoch(data)
  }
)

# extract fitted data
fitted.fitEpidemics <- function(obj) {
  data <- data.frame()
  obj <- obj@epidemics
  for (epidemic in names(obj)) {
    fit <- obj[[epidemic]]
    d <- rbind(data.frame(time = fit@time,
                          deaths = fit@deaths,
                          lower = NA,
                          upper = NA,
                          type = "raw",
                          epidemic = epidemic),
               cbind(fitted(fit),
                     type = "fitted",
                     epidemic = epidemic))
    d$time <- d$time - as.numeric(epidemic)
    data <- rbind(data, d)
  }
  data
}

#' pick the window from data for a specific epidemic (epid)
window.fitEpidemics <- function(fit, weekly = TRUE) {
  ## convert time to date
  getDate <- function(time, epid) {
    d <- subset(plague, outbreak.year==epid)
    ind <- which(d$time == time)
    sprintf("%s %2d, %d", 
            month.abb[d[ind, "month"]], 
            d[ind, "day"], 
            d[ind, "year"])
  }
  data <- data.frame()
  fit <- fit@epidemics
  for (epid in names(fit)) {
    w <- window(fit[[epid]])
    l <- length(w)
    first <- getDate(w[1], epid)
    last <- getDate(tail(w, 1), epid)
    str <- paste(first, "--", last, sep="")
    if (weekly) {
      p <- subset(plague, outbreak.year == epid)
      if (p$aggregation[[1]] == "monthly") {
        str <- NA
        l <- NA
      }
    }
    data <- rbind(data, data.frame(window = str, 
                                   n = l,
                                   epidemic = epid))
  }
  data
}
