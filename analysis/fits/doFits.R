library(epigrowthfitPNAS)
library(optimx)
library(dfoptim)

## Default arguments
def_args <- list(model="logistic",
                 max_theta=1e4,
                 epitype="major",
                 optimizer="nlminb",
                 optimfun=NULL,
                 optCtrl=list(eval.max=1000,iter.max=1000),
                 method="Nelder-Mead", ## ignored unless optimizer="optim"
                 x0link="atan",
                 start_list=NULL
                 )

for (i in seq_along(def_args)) {
    n <- names(def_args)[i]
    if (!exists(n)) assign(n,def_args[[i]])
}

## epitype should be set upstream (by make) or else in arguments

if (!exists("epitype")) stop("epitype must be defined")
cat(sprintf("fitting %s epidemics (raw data)\n",epitype))

options(epigrowthfit.model=model)

## set window_defs to beginning of outbreak year, except 1593 and 1625
##  (hand-set)
window_defs <- read.csv("window_defs.csv")
window_starts <- with(window_defs,
	ifelse(outbreak.year %in% c(1593,1625),window.start,start)
)

## fit.epidemics() depends on *NAMES* of first= argument
names(window_starts) <- window_defs$outbreak.year

london_bills_sub = dplyr::filter(london_bills
	, (severity == epitype)
	, (outbreak.year<=1665)
)
canterbury_wills_sub = dplyr::filter(canterbury_wills, severity == epitype)
london_parish_sub = dplyr::filter(london_parish, severity == epitype)

options(epigrowthfit.optimizer=optimizer,
        epigrowthfit.optCtrl=optCtrl,
        epigrowthfit.method=method,
        epigrowthfit.optimfun=optimfun)

## slightly different settings for each data set (ugh)
mnoconst <- get_model(model,link_vals=c(x0=x0link))
mconst <- get_model(model,add_const=TRUE,link_vals=c(x0=x0link))
## London bills: plague deaths only, no need for baseline
## (but use data-driven window start rather than starting from
##   beginning of outbreak year)
LBOM_arglist <- list(
    max_theta=max_theta,
    drop_mle2_call=FALSE,
    model=mnoconst)  ## can use mostly defaults
## Canterbury wills data: not aggregated, all-cause mortality
## change data, model; add baseline, window starts, deaths_var
CW_arglist <- LBOM_arglist
CW_arglist$model <- mconst
CW_arglist <- c(CW_arglist,
                list(deaths_var="nwills",
                     fixed_baseline=TRUE,
                     first_time=window_starts))
## parish register data: all-cause mortality, aggregated to weekly
## change data, deaths_var
LP_arglist <- CW_arglist
LP_arglist$deaths_var <- "total"
## Husting wills:
## change deaths_var and start
HW_arglist <- CW_arglist
HW_arglist$first_time <- NULL
HW_arglist$first <- 1

data_list=list("London bills"=london_bills_sub,
               "Canterbury wills"=canterbury_wills_sub,
               "London parish"=london_parish_sub,
               "Husting wills"=husting_wills)
args_list <- list("London bills"=LBOM_arglist,
                  "Canterbury wills"=CW_arglist,
                  "London parish"=LP_arglist,
                  "Husting wills"=HW_arglist)

if (is.null(start_list)) {
    start_list <- setNames(replicate(length(data_list), NULL),
                           names(data_list))
}


ffun <- function(data,args,theta0_list,name="") {
    cat("** ",name,"\n")
    do.call(fit.epidemics,c(list(data),args,list(theta0_list=theta0_list)))
}

s0 <- c("London bills", "Canterbury wills", "London parish")
fitList <- Map(ffun, data_list[s0], args_list[s0], start_list[s0],
               s0)

## no minor epidemics for Husting ... only do it if major
if (epitype=="major") {
    fitList <- c(fitList,
                 list("Husting wills"=
                          ffun(data_list[["Husting wills"]],
                               args_list[["Husting wills"]],
                               start_list[["Husting wills"]],
                               "Husting wills")))
}

## if (output != "") save(file=output,fitList)

