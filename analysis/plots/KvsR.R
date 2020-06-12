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

Kratfun <- function(Rratio, aKR1=1) {
  return((-1/(aKR1))*log(1 - Rratio*(1 - exp(-aKR1))))
}
##Rratio <- seq(1.01,1.6/1.1,length=100)
## Rratio <- seq(1.01, 1.9,length=1000)
Rratio <- seq(1.01, 2.1,length=1000)
Krat <- Kratfun(Rratio, aKR1=1)
plot(Rratio, Krat, type="n", bty="L", las=1,
     ylim=c(1,3),
     ##xlab=expression(R[0][2] / R[0][1]), ylab=expression(K[R][2] / K[R][1]))
     xlab="${\\mathcal R}_{0,2}/{\\mathcal R}_{0,1}$",
     ylab="$K_{\\rm R,2}/K_{\\rm R,1}$")
grid()
lines(Rratio, Krat, lwd=4)
Krat <- Kratfun(Rratio, aKR1=2)
lines(Rratio, Krat, lwd=4, col="blue")
## approx:
lines(Rratio, Rratio, lwd=4, col="red", lty="dotted")
## exact:
Krat <- Kratfun(Rratio, aKR1=0.5)
lines(Rratio, Krat, lwd=4, col="red")
titlestring <- "$a \\, K_{\\rm R,1}$"
legend("topleft",bty="n", title=titlestring, lwd=4, 
       col=c("blue","black","red"), legend=c(2,1,0.5))

## minimum rat density ratio is same as R ratio
## FRYed from supp.Rnw
ratR0 <- function(r){ (r / 365 * 18) + 1 }
r1 <- rvals["fit","early"]
r2 <- rvals["fit","late"]
R01 <- ratR0(r1)
R02 <- ratR0(r2)
R0ratio <- R02/R01

lines(c(R0ratio,R0ratio), c(R0ratio,4), col="black", lwd=12)
points(R0ratio,R0ratio, pch=21, cex=2, bg="yellow", lwd=3)
lines(c(R0ratio,R0ratio), c(R0ratio,4), col="yellow", lwd=6)

dev.off()
