library(epigrowthfitPNAS)
library(dplyr)
library(tikzDevice)

texname = paste0(rtargetname, ".tex")
args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) texname else args[1]
standAlone = if (length(args) < 2) TRUE else as.logical(args[2])
width = if (length(args) < 3) 6 else as.numeric(args[3])
height = if (length(args) < 4) 4 else as.numeric(args[4])
cat(width,height,"\n")

tikz(file, width=width, height=height,
                         standAlone=standAlone)

pop <- (pop_london
    %>% select(year, low, high, total)
    %>% filter(year >= 1300 & year <= 1700)
)

## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
par(mar=c(3,5,2,2))

## should avoid scientific notation in tick labels:
## https://stackoverflow.com/questions/5963047/do-not-want-scientific-notation-on-plot-axis

plot(total ~ year, data=pop, type="n",
     ylim=c(0,470000), yaxs="i",
     bty="L", las=1, 
     xlab="",
     ylab="",
     yaxt="n", # make my own with nice tick labels
     main="Estimated human population of London"
     )
axis(side=2, at=(0:4)*10^5, labels=c("0","$10^5$", paste("$", 2:4, "\\times 10^5$")), las=1)

##grid()
abline(h=1e5*(1:4), v=seq(1350,1700,by=50), col="grey", lty="dotted")

lines(total ~ year, data=pop,
     type="o", lwd=2, pch=21, bg="cyan", cex=2, xpd=NA
     )

##lines(low ~ year, data=pop, lty="dotted", col="grey")
##lines(high ~ year, data=pop, lty="dotted", col="grey")

## Data from Creighton 1891, Vol 1, p. 660:
creighton <- data.frame( year=c(1603,1625,1665), total=c(250000,320000,460000) )

lines(total ~ year, data=creighton,
     type="o", lwd=2, pch=21, bg="grey", cex=2, xpd=NA
     )

legend("topleft", bty="n",
       legend=c("Creighton", "Finlay"),
       lwd=2, pch=21, pt.bg=c("grey", "cyan"), pt.cex=2)

dev.off()
