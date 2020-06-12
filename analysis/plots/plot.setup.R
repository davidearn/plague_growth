library(tikzDevice)

#' plot setup function
#' @param plot.file file name
#' @param width width
#' @param height height
#' @param pointsize pointsize
#' @param theme ggplot theme
#' @
#' @importFrom tikzDevice tikz
#' @importFrom tools file_ext
#' @importFrom grDevices dev.off pdf png
#' @usage plot.setup(plot.file, width=5, height=4, pointsize=12)
#' @export plot.setup
plot.setup <- function(plot.file, width=5, height=4, pointsize=12,
                       theme = theme_bw,
                       pal = "Set1"
                       ) {
		  dev.off()
        theme_set(theme_bw())
        scale_colour_discrete <- function(...,palette=pal) {
            scale_colour_brewer(...,palette=pal)
        }
	if (plot.file == "") return()
	ext = tail(strsplit(plot.file, "\\.")[[1]], n=1)
	switch (ext,
	        pdf = pdf(plot.file, width=width, height=height, pointsize=pointsize),
	        tex = tikz(plot.file, standAlone=TRUE, width=width, height=height, 
                     pointsize=pointsize),
	        png = png(plot.file, width=width, height=height, units="in", 
	                  pointsize=pointsize)
	)
}

#' plot closing function
#' @param plot.file file name
#' @export plot.close
plot.close <- function(plot.file) {
	if (plot.file != "") dev.off()
}
