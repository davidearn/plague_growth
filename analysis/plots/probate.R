library(willsr)

## probate dates and times in husting_wills_individual are not clean/complete
ww <- (willsr::wills
	%>% select(ProbateDate)
	%>% mutate(time=lubridate::decimal_date(ProbateDate))
)
saveRDS(ww, file = rdsname)
