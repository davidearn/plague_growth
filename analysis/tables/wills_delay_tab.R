args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) "wills_delay_tab.tex" else args[1]


library(xtable)
library(dplyr)
## leave out second line (\\newline) stuff/shorten
## labels for now; will only work
## with paragraph column format (e.g. p{3cm}), but I'm not sure how
## that will play with the formatting of the rest of the column. I
## think if I were coding this directly I would use \multicolumn{}
## to modify the header format ... ?
## https://tex.stackexchange.com/questions/152066/color-first-row-in-a-xtable-generated-table
res <- (readRDS("../fits/wills_delay.rds")
    %>% rename("outbreak year"=outbreak.year,
               ## "max \\newline cross-correlation"=cor_max,
               "max CCF"=cor_max,
               ## "CCF lag \\newline (rel. to LBoM)"=LBoM_lag,
               "CCF lag"=LBoM_lag,
               "peak"=smooth_peak,
               ## "peak lag \\newline (rel. to LBoM)"=peak_diff)
               "peak lag"=peak_diff)
)

rws <- which(res$source=="wills")-1
col <- rep("\\rowcolor[gray]{0.9}", length(rws))
cat("% created by analysis/tables/wills_delay_tab.R\n", file=file)
table <- xtable(res)

## align(table) = "|l|l|l|l|l|l|l|"
## n.b.: this breaks things:
display(table) <- c("s","s","d","d","f","f","f")
digits(table) <- c(NA,NA,0,0,3,2,2)
## https://stackoverflow.com/questions/11109489/xtable-header-manipulation
## help?
print(table,
      append = FALSE,
      floating = FALSE,
      include.rownames = FALSE, 
      comment = TRUE, 
      sanitize.text.function = function(x) {x},
      file=file,
      add.to.row = list(pos = as.list(rws), command = col))


