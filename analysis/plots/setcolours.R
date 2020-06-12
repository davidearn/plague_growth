## FIX: the source name vector defined here is not a colour setting...

## colours etc. for points:
library(RColorBrewer)
mycolourpal <- brewer.pal(12, "Paired")
mygrey <- grey(0.3) # hex string representation
colvec1 <- c(parish=mycolourpal[2],
            LBoM=mycolourpal[6],
            wills=mygrey)
pchvec1 <- c(parish=16,LBoM=17,wills=15)
long_names <- c(wills="PCC Wills",parish="Parish registers",
                LBoM="Plague deaths")

## legend order
vec_order <- c("wills","parish","LBoM")
colvec1 <- colvec1[vec_order]
long_names <- long_names[vec_order]
pchvec1 <- pchvec1[vec_order]

## ggplot doesn't like mix of hex and named colours, and
## doesn't like the colvec elements to be named!
## order here is parish, LBoM, wills:
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
col2hex <- function(col) rgb2hex(t(col2rgb(col)))
colvec <- col2hex(colvec1)
## Note: the above fanciness is not necessary if colvec1
##       is all hex.  But it is necessary if colvec1 is a
##       mixture of hex and named colours like "black".

## The above works as desired, but Ben notes:
## if you want to get the order right using
##      scale_colour_manual(breaks=colvecnames,values=colvec)
## you need to specify a breaks= vector that matches the names in
## data.source and is in the right order ...
## Hence: the order in this source name vector matters!
mysource.name <- c(acm.parish.const="Parish registers",
                   plague.deaths.LBoM="Plague deaths",
                   nwills.const="Wills")
