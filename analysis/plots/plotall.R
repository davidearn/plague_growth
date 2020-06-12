library(epigrowthfit)
library(ggplot2)
library(tikzDevice)

args = commandArgs(trailingOnly=TRUE)
file = pdfname
file = paste0(rtargetname, ".tex")

data = subset(plague, aggregation=="weekly" & place == "London")
figure <- ggplot(data=data, mapping=aes(x=time, y=plague.deaths, 
                                        color=severity, group=outbreak.year)) +
    scale_y_continuous("weekly plague deaths",
                       expand=c(0,0)) + # multiplicative and additive expansion of scale
    expand_limits(y=7500) +
    ##scale_x_continuous("year")+
    scale_x_continuous("")+
    geom_line() +
    scale_colour_brewer(palette="Set1") # first colour is red

plot.setup(file, width=4, height=2)
print(figure)
plot.close(file)
