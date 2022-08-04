library(epigrowthfitPNAS)
library(ggplot2)
library(tikzDevice)

file = pdfname
file = paste0(rtargetname, ".tex")

data = subset(plague, aggregation=="weekly" & place == "London")
data$scaled = 1000*data$plague.deaths/data$population
figure <- ggplot(data=data, mapping=aes(x=time, y=scaled, color=severity, group=outbreak.year)) +
    ##scale_y_continuous("weekly plague/other causes deaths") +
    ##scale_y_continuous("weekly plague deaths \\,\\emph{per capita}") +
    scale_y_continuous("weekly plague deaths\nper 1000 population",
                       expand=c(0,0)) + # multiplicative and additive expansion of scale
    expand_limits(y=19.9) +
    ##scale_x_continuous("year")+
    scale_x_continuous("")+
    geom_line() +
    scale_colour_brewer(palette="Set1") # first colour is red

plot.setup(file, width=4, height=2)
print(figure)
plot.close(file)

## get peak values
majors <- c(1563, 1593, 1603, 1625, 1665)
nmajors <- length(majors)
NAs <- rep(NA,nmajors)
dfpeaks <- data.frame(row.names=majors, peak=NAs, scaled=NAs)
for (oy in majors) {
  dd <- subset(data,outbreak.year==oy)
  this.peak <- max(dd$plague.deaths, na.rm=TRUE)
  this.peak.scaled <- max(dd$scaled, na.rm=TRUE)
  dfpeaks[as.character(oy),] <- c(this.peak, this.peak.scaled)
}
print(dfpeaks)
