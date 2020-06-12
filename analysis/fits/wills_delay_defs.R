args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) "wills_delay_defs.tex" else args[1]

library(dplyr)
res <- readRDS(input_files[[1]])

parish_cormax_range <- res %>% filter(source=="parish") %>% pull(cor_max) %>% range()
parish_ccflag_range <- res %>% filter(source=="parish") %>% pull(LBoM_lag) %>% range()
parish_peaklag_range <- res %>% filter(source=="parish") %>% pull(peak_diff) %>% range()

wills_cormax_range <- res %>% filter(source=="wills") %>% pull(cor_max) %>% range()
wills_ccflag_range <- res %>% filter(source=="wills") %>% pull(LBoM_lag) %>% range()
wills_peaklag_range <- res %>% filter(source=="wills") %>% pull(peak_diff) %>% range()

catt <- function(...,f=file) {
    cat(...,file=f,append=TRUE)
}

latexout <- function(x,nm,fmt="%1.1f") {
    catt(sprintf("\\newcommand{\\%s}{%s}\n",nm,sprintf(fmt,x)))
}

unlink(file)

catt("% parish range of max CCF with LBoM\n")
latexout(parish_cormax_range[1],"parishccfmaxmin","%1.2f")
latexout(parish_cormax_range[2],"parishccfmaxmax","%1.3f")
catt("% parish range of CCF lag from LBoM\n")
latexout(parish_ccflag_range[1],"parishccflagmin","%d")
latexout(parish_ccflag_range[2],"parishccflagmax","%d")
catt("% parish range of peak lag from LBoM\n")
latexout(parish_peaklag_range[1],"parishpeaklagmin")
latexout(parish_peaklag_range[2],"parishpeaklagmax")

catt("% wills range of max CCF with LBoM\n")
latexout(wills_cormax_range[1],"willsccfmaxmin","%1.2f")
latexout(wills_cormax_range[2],"willsccfmaxmax","%1.2f")
catt("% wills range of CCF lag from LBoM\n")
latexout(wills_ccflag_range[1],"willsccflagmin","%d")
latexout(wills_ccflag_range[2],"willsccflagmax","%d")
catt("% wills range of peak lag from LBoM\n")
latexout(wills_peaklag_range[1],"willspeaklagmin")
latexout(wills_peaklag_range[2],"willspeaklagmax")
