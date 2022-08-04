p <- scan("required_packages", comment="#", what=character(0), quiet=TRUE)
p <- c(p, "epigrowthfitPNAS")
res <- suppressPackageStartupMessages(
    vapply(p, require, quietly = TRUE, character.only=TRUE, logical(1))
    )
if (any(!res)) {
    missing <- p[!res]
    stop("please install these missing packages: ",
         paste(missing, collapse=", "))
}

