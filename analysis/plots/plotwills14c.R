## show 14th century Husting wills in style of plot3sourcesmajors.R

library(epigrowthfitPNAS)
library(dplyr)
library(lubridate)
library(tikzDevice)

texname = paste0(rtargetname, ".tex")
args = commandArgs(trailingOnly=TRUE)
file = if (length(args) < 1) texname else args[1]

aggWeeks <- 1
aggUnit <- paste(aggWeeks,"weeks")
myylab <- if (aggUnit=="1 weeks") "" else paste(aggWeeks, "-weekly counts")

wills <- aggregate_wills(husting_wills_individual,
                         include_all=TRUE, aggregation=aggUnit)

tmin <- 1300
tmax <- 1400
ymax <- max(wills[,"nwills"], na.rm=TRUE)
ymin <- 0.9

## REMIND MYSELF OF THE pch VALUES
if (interactive()) {
  vec <- 1:25
  plot(vec,vec,type="n")
  grid()
  points(vec,vec,pch=vec,cex=2)
}

majors <- c(1348,1361,1368,1375)
is.major <- function(time) {
  res <- rep(FALSE, length(time))
  for (yr in majors) {
    res <- res | (time > yr & time < yr+1)
  }
  return(res)
}

## get ymin and ymax for each major epidemic:
yrange_defs <- epidemic_defs %>%
  filter(severity=="major", outbreak.year<1500) %>%
  mutate(wmin=NA,wmax=NA,ymin=NA,ymax=NA)
for (yr in majors) {
  iyr <- which(yrange_defs[,"outbreak.year"]==yr)
  t1 <- yrange_defs[iyr,"start"]
  t2 <- yrange_defs[iyr,"end"]
  wtmp <- wills %>% filter(time >= t1 & time <= t2)
  w1 <- min(wtmp[["nwills"]], na.rm=TRUE)
  w2 <- max(wtmp[["nwills"]], na.rm=TRUE)
  y1 <- w1
  y2 <- w2
  yrange_defs[iyr,"wmin"] <- w1
  yrange_defs[iyr,"wmax"] <- w2
  yrange_defs[iyr,"ymin"] <- y1
  yrange_defs[iyr,"ymax"] <- y2
}
print(yrange_defs)

## MAKE THE PLOT

if (!interactive()) tikz(file, width=5, height=6.5, standAlone=TRUE)

## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
mar.orig <- par("mar")
names(mar.orig) <- c("bottom", "left", "top", "right")
mymar <- mar.orig
mymar["left"] <- mymar["left"] * 1
mymar["right"] <- mymar["right"]*1
mymar["top"] <- mymar["top"]*0.1
mymar["bottom"] <- mymar["bottom"]*0.5
par(mar=mymar)

cex <- 2
cex.text <- 1

colvec <- colvec1

alphavec <- c(wills=0.8)
mycol <- function(colourname) {
  return(adjustcolor(colvec[colourname], alpha.f=alphavec[colourname]))
}

mylog <- ""

par(mfrow=c(length(majors),1))

for (yr in majors) {
  
  iyr <- which(yrange_defs[,"outbreak.year"]==yr)
  if (yr==1348) yr <- yr + 1 # shift left by one year
  tmin <- yr - 0.5
  tmax <- yr + 1.5
  if (mylog != "y") {
    ymin <- 0 ##yrange_defs[iyr,"ymin"]
    ymax <- yrange_defs[iyr,"ymax"]
  }
  cat("yr =", yr, "\ttmin =", tmin, "\ttmax =", tmax,
      "\tymin =", ymin, "\tymax =", ymax, "\n")

  plot(NA,NA, xlim=c(tmin,tmax), ylim=c(ymin,ymax*1.05), log=mylog, bty="U",
       xlab="", ylab=myylab, xaxs="i", yaxs="i", las=1, xaxt="n",
       cex.axis=cex.text*1.3, cex.lab=cex.text)
  
  ## x-axis:
  xmonthsDates <- as.Date(sprintf("%d-%2.2d-01",yr-1,1:12))
  xmonthsDates <- c(xmonthsDates, as.Date(sprintf("%d-%2.2d-01",yr,1:12)))
  xmonthsDates <- c(xmonthsDates, as.Date(sprintf("%d-%2.2d-01",yr+1,1:12)))
  xmonths <- decimal_date(xmonthsDates)
  axis(side=1, at=xmonths, labels=format(xmonthsDates,format="%b"), srt=45)
  ## now add year labels:
  janDates <- as.Date(sprintf("%d-01-01",c(yr,yr+1)))
  jans <- decimal_date(janDates)
  mtext(as.character(c(yr,yr+1)), side=1, at=jans, line=0.6,
        col="red", cex=0.8, las=2)

  ## mark epidemic def
  t1 <- yrange_defs[iyr,"start"]
  t2 <- yrange_defs[iyr,"end"]
  abline(v=c(t1,t2), col="grey")

  w <- wills[["nwills"]]
  wmin <- 0 ###yrange_defs[iyr,"wmin"]
  wmax <- yrange_defs[iyr,"wmax"]
  if (mylog != "y") {# spread wills out to full y range
    w <- (w-wmin)*(ymax-ymin)/(wmax-wmin)
  }
  pch <- 15 # 15 # 22 # square
  with(wills, points(time, w, pch=pch, col=mycol("wills"), type="o", lwd=3, cex=cex*0.75))

  ## axis on RHS for wills
##  wby <- if (wmax<=20) 5 else 10
##  wticks <- seq(0,wmax,by=wby)
##  yticks <- wticks*ymax/wmax
##  axis(side=4, at=yticks, labels=wticks, las=1)
  axis(side=4, labels=FALSE)

  ## epidemic labels
  ## (not really needed if we are plotting year at each Jan 1st)
  if (yr==1349) yr <- yr - 1 # so label is correct for shifted case
  legend("topright",bty="n",legend=sprintf("{\\bfseries%d\\quad\\null}",yr),
         cex=cex.text*1.5,text.col="red")
  
  ## data legend
  if (yr<1350) {
    xlims <- par("usr")[1:2]
    ylims <- par("usr")[3:4]
    x <- xlims[1] + 0.05*(xlims[2]-xlims[1])
    y <- ylims[1] + 0.95*(ylims[2]-ylims[1])
    legend.text <- c("Husting Wills")
    mycolvec <- c(mycol("wills"))
    legend(##x,y,
           "topleft", pch=c(15), lty=c(1), lwd=c(3), cex=1.3,
           legend=legend.text, col=mycolvec, bty="n")
  }
}

if (!interactive()) dev.off()
