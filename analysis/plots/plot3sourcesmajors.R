## show bills, wills and parish registered so we can see the major
## epidemics are evident simultaneously no matter what source we
## examine

library(epigrowthfit)
library(dplyr)
library(lubridate)
library(tikzDevice)

args = commandArgs(trailingOnly=TRUE)
texname <- paste0(rtargetname, ".tex")
file = if (length(args) < 1) texname else args[1]

aggWeeks <- 1
aggUnit <- paste(aggWeeks,"weeks")
myylab <- if (aggUnit=="1 weeks") "" else paste(aggWeeks, "-weekly counts")

wills <- aggregate_wills(canterbury_wills_individual,
                         include_all=TRUE, aggregation=aggUnit)
LBoMvar <- "plague.deaths"
##LBoMvar <- "all.cause.deaths"
LBoM <- london_bills[,c("time",LBoMvar)]
parishVar <- "total"
parish <- london_parish_all[,c("time",parishVar)]

if (aggWeeks > 1) {
  ## we need to aggregate the LBoM and parish time series further
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

## REMIND MYSELF OF THE pch VALUES
if (interactive()) {
  vec <- 1:25
  plot(vec,vec,type="n")
  grid()
  points(vec,vec,pch=vec,cex=2)
}

majors <- c(1563, 1593, 1603, 1625, 1665)
is.major <- function(time) {
  res <- rep(FALSE, length(time))
  for (yr in majors) {
    res <- res | (time > yr & time < yr+1)
  }
  return(res)
}

## get ymin and ymax for each major epidemic:
yrange_defs <- epidemic_defs %>%
  filter(severity=="major", outbreak.year>1500) %>%
  mutate(wmin=NA,wmax=NA,ymin=NA,ymax=NA)
for (yr in majors) {
  iyr <- which(yrange_defs[,"outbreak.year"]==yr)
  t1 <- yrange_defs[iyr,"start"]
  t2 <- yrange_defs[iyr,"end"]
  ltmp <- LBoM %>% filter(time >= t1 & time <= t2)
  ptmp <- parish %>% filter(time >= t1 & time <= t2)
  wtmp <- wills %>% filter(time >= t1 & time <= t2)
  w1 <- min(wtmp[["nwills"]], na.rm=TRUE)
  w2 <- max(wtmp[["nwills"]], na.rm=TRUE)
  if (yr < 1400) {
    y1 <- w1
    y2 <- w2
  } else {
    y1 <- min(ltmp[[LBoMvar]], ptmp[[parishVar]], wtmp[["nwills"]], na.rm=TRUE)
    y2 <- max(ltmp[[LBoMvar]], ptmp[[parishVar]], wtmp[["nwills"]], na.rm=TRUE)
  }
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

alphavec <- c(parish=0.9,
            LBoM=0.8,
            wills=0.8)
mycol <- function(colourname) {
  return(adjustcolor(colvec[colourname], alpha.f=alphavec[colourname]))
}

mylog <- ""

par(mfrow=c(length(majors),1))

for (yr in majors) {
  
  iyr <- which(yrange_defs[,"outbreak.year"]==yr)
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
    with(wills, points(time, w,
                       pch=pchvec1["wills"],
                       col=mycol("wills"), type="o", lwd=3, cex=cex*0.75))

  t1 <- yrange_defs[iyr,"start"]
  t2 <- yrange_defs[iyr,"end"]
  ltmp <- LBoM %>% filter(time >= t1 & time <= t2)
    with(ltmp, points(time, ltmp[,LBoMvar],
                      pch=pchvec1["LBoM"],
                      col=mycol("LBoM"),
                      type="o", cex=cex))

    with(parish, points(time, total,
                        pch=pchvec1["parish"],
                        col=mycol("parish"), type="o", lwd=4, cex=cex))

  ## axis on RHS for wills
  wby <- if (wmax<=20) 5 else 10
  wticks <- seq(0,wmax,by=wby)
  yticks <- wticks*ymax/wmax
  axis(side=4, at=yticks, labels=wticks, las=1)

  ## epidemic labels
  ## (not really needed if we are plotting year at each Jan 1st)
  legend("topright",bty="n",legend=sprintf("{\\bfseries%d\\quad\\null}",yr),
         cex=cex.text*1.5,text.col="red")
  
  ## data legend
  if (yr==1563) {
    xlims <- par("usr")[1:2]
    ylims <- par("usr")[3:4]
    x <- xlims[1] + 0.05*(xlims[2]-xlims[1])
    y <- ylims[1] + 0.95*(ylims[2]-ylims[1])
    legend.text <- long_names
    mycolvec <- sapply(names(colvec1),mycol)
    lwdvec <- c(parish=4,LBoM=1,wills=3)
    lwdvec <- lwdvec[names(colvec1)]
    legend(##x,y,
        "topleft", pch=pchvec1,
        lty=c(1,1,1), lwd=lwdvec, cex=1.3,
        legend=legend.text, col=mycolvec, bty="n")
  }
}

if (!interactive()) dev.off()
