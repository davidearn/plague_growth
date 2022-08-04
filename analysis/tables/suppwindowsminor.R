args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) "suppwindowsminor.tex" else args[1]
majors_only = if (length(args) < 2) TRUE else as.logical(args[2])

library(Hmisc) # for latex()
library(epigrowthfitPNAS)
library(dplyr)

window_defs <- (summary.fitList(fitList,key="type")
    %>% select(type,outbreak.year,
               fulltime.min,fulltime.max,
               wintime.min,wintime.max)
    %>% arrange(fulltime.min)  ## arrange BEFORE converting to char!
    %>% mutate_at(grep("\\.(min|max)",names(.)),
                  funs(format(lubridate::date_decimal(.),"%d %b %Y")))
    %>% rename("outbreak year"=outbreak.year,
               "fitting window (start)"=wintime.min,
               "fitting window (end)"=wintime.max,
               "outbreak window (start)"=fulltime.min,
               "outbreak window (end)"=fulltime.max)
)
    

cat("% created by analysis/tables/suppwindows.R\n", file=file)

ltx <- latex(window_defs,
      multicol=TRUE, # allow \multicolumn in header
      rowname=NULL,
      cgroup=c("", "Outbreak Window", "Fitting Window"),
      colheads=c("Source","Outbreak Year", "start", "end", "start", "end"),
      col.just = c("l","c",rep("r",4)),
      table.env=FALSE,
      file=file
      )
