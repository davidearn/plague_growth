## install all packages needed by library.R

## install packages listed in required_packages file from CRAN
options(repos=c(CRAN="https://cloud.r-project.org"))
pkgs <- scan("required_packages", comment="#", what=character(0), quiet=TRUE)
i1 <- installed.packages()
pkgs <- pkgs[!pkgs %in% rownames(i1)]
install.packages(pkgs)

## install the correct version of bbmle??
library(remotes)
install_github("bbolker/bbmle")

## Install an older version of epigrowthfit (provides the package epigrowthfitPNAS)
##     install_github("davidearn/epigrowthfit",dependencies=TRUE)
## but this current version is not backwards-compatible and will not
## work with the code for the PNAS plague growth paper.  Therefore, we
## install the version that was released with the PNAS paper:
install_github("davidearn/epigrowthfit",
               ref = "pnas",
               dependencies = TRUE,
               build_vignettes = TRUE)			
