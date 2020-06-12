#' testing 3-axis solutions
#' cf. cowplot::axis_canvas,
library(cowplot)
library(gtable)
#' @examples
#' set.seed(101)
#' dd <- data.frame(x=rnorm(20),y=rnorm(20))
#' library(ggplot2)
#' gg0 <- ggplot(dd)
#' g1A <- gg0 + geom_point(aes(x,y))
#' g1B <- gg0 + geom_point(aes(x,10*y))
#' g1C <- gg0 + geom_point(aes(x,10*y)) + geom_vline(xintercept=-Inf)
#' g12 <- combine_axes(g1A,g1C,add_pos="l")
#' print(g12)  ## only need plot() if we return a gtable
#' g12T <- combine_axes(g0,g1,add_pos="l",return_gtable=TRUE)
#' g123T <- combine_axes(g12T,g1,add_pos="l")
#' g12L <- combine_axes(g2,g1,add_pos="l",pad_inner=5)
#' g12LS <- combine_axes(g2,g1,add_pos="l",pad_inner=3)
#' print(g12L)
#' @param extra_plot ggplot with elements to extract & paste
#' @param base_plot ggplot or gtable to extend
#' @param pattern regexp of elements to extract and attach; if not specified, look for axis and ylab on the plot side corresponding to \code{add_pos}
#' @param add_pos which side to add elements to?
#' @param tag ??
#' @param pad ??
#' @param pad_inner ??
#' to right-hand side (at the moment) of base_plot
combine_axes <- function(base_plot,extra_plot,
                         pattern=NULL,
                         add_pos="r",
                         tag="1",
                         pad=NULL,
                         pad_inner=NULL,
                         debug=FALSE,
                         return_gtable=FALSE,
                         xtheme=NULL) {
    if (is.null(pattern)) {
        pattern <- sprintf("(axis-%s|ylab-%s)",add_pos,add_pos)
    }
    if (!inherits(base_plot,"gtable")) {
        x0 <- cowplot::plot_to_gtable(base_plot)
    } else {
        x0 <- base_plot
    }
    ## add space before axis
    if (!is.null(pad_inner)) {
        x0 <- gtable_add_cols(x0,
                   pos=if (add_pos=="l") 0 else -1,
                   widths=grid::unit(pad_inner,"cm"))
    }
    base_inds <- grep(pattern, x0$layout$name)
    ## vertical extents should be the same as in the old plot
    bottom <- min(x0$layout$b[base_inds])
    top <- max(x0$layout$t[base_inds])
    x <- cowplot::plot_to_gtable(extra_plot)
    ## find focal element(s)
    inds <- grep(pattern, x$layout$name)
    if (length(inds)==0) stop('pattern not found')
    ## find focal column(s)
    left <- min(x$layout$l[inds])
    right <- max(x$layout$r[inds])
    p <- if (is.null(pad_inner)) 0 else 1
    ## figure out where to add new stuff;
    if (add_pos=="l") {
        ext_pos <- min(x0$layout$l)-p
    } else { ## assume "r"
        ext_pos <- max(x0$layout$r)+p
    }
    ## grab all elements in focal columns
    all_inds <- which(x$layout$l>=left & x$layout$r<=right)
    ## grab elements from x and duplicate them
    newcol <- list(grobs=x$grobs[all_inds],
                   name=paste(x$layout$name[all_inds],tag,sep="_"),
                   layout=x$layout[all_inds, , drop=FALSE])

    ## add new columns to x0
    if (is.null(pad)) {
        pad <- grid::unit(rep(0,right-left+1),
                          rep("cm",right-left+1))
    }
    x0 <- gtable_add_cols(x0,width=x$width[left:right]+pad,
                          pos=switch(add_pos,l=0,r=-1))
    new_L <- ext_pos+newcol$layout$l-left
    new_R <- ext_pos+newcol$layout$r-left
    ## stick objects from x into x0
    if (debug) {
        cat("L-R columns in new plot:",all_inds,"\n")
        cat("L positions:",new_L,"\n")
    }
    x0 <- gtable_add_grob(x0,
                          newcol$grobs,
                          l=new_L,
                          r=new_R,
                          t=top,
                          b=bottom,
                          name=newcol$name,
                          clip = "off" ## "inherit"
                          )
    ## extend background
    bkg_ind <- grep("background", x0$layout$name)
    x0$layout$l[bkg_ind] <- min(x0$layout$l)
    x0$layout$r[bkg_ind] <- max(x0$layout$r)
    ## z_normalise  (with i=1)
    x0$layout$z <- rank(x0$layout$z, ties.method = "first")
    if (!return_gtable) {
        x0 <- ggdraw(x0)
        if (!is.null(xtheme)) {
            x0 <- x0 + xtheme
        }
    }
    return(x0)
}

### EXPERIMENTING
if (FALSE) {
    dd <- data.frame(x=rnorm(20),y=rnorm(20))
    library(ggplot2)
    gg0 <- ggplot(dd)
    g1A <- gg0 + geom_point(aes(x,y))
    g1B <- (gg0
        + geom_blank(aes(NA,10*y))
        + geom_vline(aes(xintercept=-Inf))
        + labs(x="")
        + scale_x_discrete(breaks=NULL)
    )
    combine_axes(g1A,g1B,pattern="(axis-l|ylab-l|panel)",add_pos="l")
    yax <- axis_canvas(g1A, axis="y") +
        geom_vline(xintercept=min(dd$x))
    ggdraw(insert_yaxis_grob(g1A,yax,position="left"))

}
