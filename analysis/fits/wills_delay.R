library(epigrowthfitPNAS)
library(dplyr)
library(tidyr)

## only have parish records so uninteresting to measure delay times
yrs <- unique(as.numeric(dd_majors$outbreak.year))
yrs <- yrs[yrs>1500 & yrs != 1593]
res <- list()
for (oy in yrs) {
    dd1 <- (dd_majors
        %>% filter(outbreak.year==as.character(oy))
        %>% select(-outbreak.year)
        %>% split(.$source)
    )
    focal <- "LBoM"
    ## interpolate to LBoM
    for (s in c("parish","wills","LBoM")) {
        if (nrow(dd1[[s]])>0) {
            ## CCF computation
            if (s!=focal) {
                adat <- ( with(dd1[[s]],
                               approx(time,deaths,xout=dd1[[focal]]$time))
                    %>% setNames(c("time","deaths"))
                    %>% as.data.frame()
                    %>% cbind(LBoM=dd1[[focal]]$deaths)
                    %>% na.omit()
                )
                cc <- ccf(adat$deaths,adat$LBoM)
                cc_peak <- cc$lag[which.max(cc$acf)]
                cc_val <- max(cc$acf)
            } else {
                ## no CCF comparison for focal source
                cc_peak <- cc_val <- NA
            }
            tvals <- seq(min(dd1[[s]]$time),max(dd1[[s]]$time),length.out=250)
            spl <- with(dd1[[s]],stats::smooth.spline(time,deaths))
            smooth_vals <- predict(spl,tvals)$y
            smooth_peak <- tvals[which.max(smooth_vals)]
            res <- c(res,
                     list(data.frame(source=s,outbreak.year=oy,
                                LBoM_lag=cc_peak,
                                cor_max=cc_val,
                                smooth_peak,
                                stringsAsFactors=FALSE)))
        }
    }
}
res <- (bind_rows(res)
    %>% group_by(outbreak.year)
    ## peak difference in weeks
    %>% mutate(peak_diff=52*(smooth_peak-smooth_peak[source=="LBoM"]))
    %>% filter(source!="LBoM")
)

saveRDS(res,file="wills_delay.rds")

