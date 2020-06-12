library(epigrowthfit)
library(dplyr)
library(lubridate)
library(tikzDevice)
library(RColorBrewer)

args = commandArgs(trailingOnly=TRUE)

texname <- paste0(rtargetname, ".tex")
## testing
## args <- c("tmp.tex","FALSE","9","7")
file = if (length(args) < 1) texname else args[1]
standAlone = if (length(args) < 2) TRUE else as.logical(args[2])
width = if (length(args) < 3) 12 else as.numeric(args[3])
height = if (length(args) < 4) 8 else as.numeric(args[4])
cat(width,height,"\n")

aggWeeks <- 4
aggUnit <- paste(aggWeeks,"weeks")
myylab <- if (aggUnit=="1 week") "Weekly counts" else paste(aggWeeks, "-weekly counts")

wills <- aggregate_wills(canterbury_wills_individual,
                         include_all=TRUE, aggregation=aggUnit)
LBoMvar <- "plague.deaths"
##LBoMvar <- "all.cause.deaths"
LBoM <- london_bills[,c("time",LBoMvar)]
parishVar <- "total"
parish <- london_parish_all[,c("time",parishVar)]

if (aggWeeks > 1) {
  ## we need to aggregate the LBoM and parish time series further
  cat("plot3sources.R: Aggregating LBoM and parish...\n")
  LBoMorig <- LBoM
  parishOrig <- parish
  parish <- with(parish,aggsum(time=time,deaths=parish[,parishVar],period=aggUnit))
  names(parish)[-1] <- names(parishOrig)
  LBoM <- with(LBoM,aggsum(time=time,deaths=LBoM[,LBoMvar],period=aggUnit))
  names(LBoM)[-1] <- names(LBoMorig)
}

tmin <- 1540
tmax <- 1680
ymax <- max(LBoM[,LBoMvar], parish$total, na.rm=TRUE)
ymin <- 0.9

make_polygon <- function(yr, col="yellow") {
  xmin <- yr - 1
  xmax <- yr + 2
  x <- c(xmin, xmax, xmax, xmin, xmin)
  y <- c(ymin, ymin, ymax, ymax, ymin)
  polygon(x, y, col=col, border=NA)
  xmid <- yr + 0.5
  ##text(xmid, ymax, label=yr, font=2, xpd=NA)
  mtext(text=yr, side=3, font=2, at=xmid, xpd=NA, cex=cex.text)
}

majors <- c(1563, 1593, 1603, 1625, 1665)
is.major <- function(time) {
  res <- rep(FALSE, length(time))
  for (yr in majors) {
    res <- res | (time > yr & time < yr+1)
  }
  return(res)
}

## MAKE THE PLOT

cat("plot3sources.R: Beginning plot...\n")
## if (!interactive()) tikz(file, width=width, height=height, standAlone=standAlone) 
tikz(file, width=width, height=height, standAlone=standAlone) 

## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
mar.orig <- par("mar")
names(mar.orig) <- c("bottom", "left", "top", "right")
mymar <- mar.orig
mymar["left"] <- mymar["left"] * 1
mymar["right"] <- mymar["right"]*0.5
mymar["top"] <- mymar["top"]*0.3
mymar["bottom"] <- mymar["bottom"]*0.5
par(mar=mymar)

cex.text <- 1
plot(NA,NA, xlim=c(tmin,tmax), ylim=c(ymin,ymax), log="y", bty="L",
     xlab="", ylab=myylab, xaxs="i", yaxs="i", las=1,
     cex.axis=cex.text*1.3, cex.lab=cex.text*1.3)

## highlight major plague years
for (yr in majors) make_polygon(yr)
## indicate 1551
fac51 <- if (aggWeeks==1) 1.1 else 0.8
text(1551.5, max(subset(parish,time<1555)$total)*fac51, label=1551, cex=cex.text)
## indicate flu epidemic of 1557-9
t1 <- decimal_date(as.Date("1557-01-01"))
t2 <- decimal_date(as.Date("1559-12-31"))
y1 <- max(subset(parish, time>t1 & time<t2)$total)
y2 <- y1*1.1
lines(c(t1,t1,t2,t2), c(y1,y2,y2,y1))
text((t1+t2)/2, y2*1.1, label="flu", cex=cex.text)
## indicate Interregnum
t1 <- decimal_date(as.Date("1649-01-30"))
t2 <- decimal_date(as.Date("1660-05-29"))
y1 <- max(subset(parish, time>t1 & time<t2)$total)
y2 <- y1*1.1
lines(c(t1,t1,t2,t2), c(y1,y2,y2,y1))
text((t1+t2)/2, y2*1.1, label="Interregnum", cex=cex.text)
##indicate date of coronation of Elizabeth I:
##abline(v=decimal_date(as.Date("1558-11-17")),col="red")

cex <- 1 ## 0.65
cex.major <- cex ##0.4 ### 1

colvec <- colvec1
pchvec1["parish"] <- NA ## not using points for parish

with(parish, points(time, total,
                    ## pch=pchvec1["parish"], ## not used (type="l")

                    col=colvec["parish"], type="l", lwd=2,
                    cex=ifelse(is.major(time),cex.major,cex)))
with(LBoM, points(time, get(LBoMvar),
                  pch=pchvec1["LBoM"],
                  col=colvec["LBoM"], xpd=NA,
                  cex=ifelse(is.major(time),cex.major+0.1,cex)))
with(wills, points(time, nwills,
                   pch=pchvec1["wills"],
                   col=colvec["wills"], type="o", lwd=0.5,
                   cex=ifelse(is.major(time),cex.major,cex)))
## emphasize wills with yellow dots:
### with(wills, points(time, nwills, pch=".", col="yellow"))

## legend
xlims <- par("usr")[1:2]
ylims <- par("usr")[3:4]
x <- xlims[1] + 0.05*(xlims[2]-xlims[1])
y <- exp(2.2*ylims[2])
legend.text <- long_names ## from setcolours
nc <- names(colvec1)
legend(
    "topleft",
    pch=pchvec1,
    lty=ifelse(nc=="LBoM",0,1),
    lwd=ifelse(nc=="parish",0.5,
        ifelse(nc=="LBoM",0,2)),
    legend=long_names,
    col=colvec, bty="n")

## if (!interactive()) dev.off()
