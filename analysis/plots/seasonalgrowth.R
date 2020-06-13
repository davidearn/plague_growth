library(epigrowthfit)
library(cowplot)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(purrr)
library(tikzDevice)

defaults = list(output="",width=7,height=4)

width <- defaults[["width"]]
height <- defaults[["height"]]

texname <- paste0(rtargetname, ".tex")

args = commandArgs(trailingOnly=TRUE)
file = pdfname
file = paste0(rtargetname, ".tex")

## hack since we want to import 'fitList' from both minors and majors ...
## minors$ to distinguish from minors_combo_logit (etc.)
## [FIXME: more thorough renaming]
minorList <- envir_list[[grep("minors$", names(envir_list))]]$fitList
minorSum <- sumfun(minorList,level=2)
majorList <- envir_list[[grep("major", names(envir_list))]]$fitList
majorSum <- sumfun(majorList,level=2)

## slightly redundant with conv info from summary/summary.fitList/sumfun
conv_info <- map_dfr(minorList,get_conv,.id="source")
conv_info$conv

bad <- (conv_info
    ## skip these fits because they are *sometimes* bad
    %>% filter((outbreak.year=="1578" & source=="London bills") |
               (outbreak.year=="1581" & source=="Canterbury wills") |
               conv==1)
)

bad$msg
if (nrow(bad)>2) {
    print(bad)
    stop("unexpected convergence failures")
}
minorList[["Canterbury wills"]][["1581"]]@mle2@details

##%>% select(source,outbreak.year,conv,msg)
par(mfrow=c(1,2))
for (i in seq(nrow(bad))) {
    plot(minorList[[bad$source[i]]][[bad$outbreak.year[[i]]]])
}

## hack since we loaded data *including pdfname*?
dd <- (bind_rows(minor=minorSum,major=majorSum,.id="severity")
    %>% filter(outbreak.year>1500)
    %>% mutate(source=mod_source(source))  ## recode source names
    %>% drop_bad()
    %>% filter(conv==0)
    %>% select(outbreak.year,source,
               severity,starts_with("growthrate"))
)

labfun <- function(brks) {
    f <- split(brks,substr(brks,1,2))
    f2 <- lapply(f,
           function(x) {
        x[-1] <- paste0("'",substr(x[-1],3,nchar(x[-1])))
		  x[[1]] <- sprintf("\\textbf{%s}", x[[1]])
        return(x)
    })
    return(unlist(f2))
}

fig <- (ggplot(dd,aes(outbreak.year,growthrate.value,
                      colour=source,shape=source,size=severity))
    + geom_pointrange(aes(ymin=growthrate.lower,ymax=growthrate.upper),
                      alpha=0.5)
    + scale_size_manual(values=c(1.5,0.75))
    + scale_colour_manual(values=colvec1)
    + scale_shape_manual(values=pchvec1)
    + scale_y_log10(limits=c(1,100))
    + scale_x_discrete(labels=labfun)
    + theme(axis.text.x=element_text(size=7))
    + labs(x="outbreak year",y="growth rate ($r$, /year)")
)

if (!interactive()) tikz(file=texname, width=width, height=height,
                         standAlone=TRUE)

print(fig)

if (!interactive()) dev.off()

