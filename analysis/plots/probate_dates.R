library(ggplot2); theme_set(theme_bw())
library(tikzDevice)

args = commandArgs(trailingOnly=TRUE)
file = pdfname
file = paste0(rtargetname, ".tex")

dd_comb <- readRDS("../../data/probate_counts.rds")
figure <- (ggplot(dd_comb,aes(date,count,colour=type,shape=type,linetype=type))
    + geom_point()
    ## + geom_smooth(method="gam",
    ## method.args=list(family=quasipoisson),
    ## formula=y~s(x,k=5))
    + geom_smooth(span=0.5,method="loess")
    + scale_y_continuous(
          trans="log1p"
                       , limits=c(0,80)
                       , expand=c(0.02,0)
                       , oob=scales::squish
                         )
    + facet_wrap(~outbreak.year,scale="free_x")
    + scale_colour_brewer(palette="Set2")
    + labs(y="count\n(log(1+x) scale)")
    + scale_x_date(labels=function(x) format(x,"%Y"))
)

plot.setup(file, width=8, height=6)
print(figure)
plot.close(file)
