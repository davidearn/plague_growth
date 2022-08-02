## install all packages needed by library.R
options(repos=c(CRAN="https://cloud.r-project.org"))
pkgs <- scan("required_packages", comment="#", what=character(0), quiet=TRUE)
i1 <- installed.packages()
pkgs <- pkgs[!pkgs %in% rownames(i1)]
install.packages(pkgs)
library(remotes)
install_github("bbolker/bbmle")
##
## The current version of epigrowthfit is installed via
##     install_github("davidearn/epigrowthfit",dependencies=TRUE)
## but this current version is not backwards-compatible and will not
## work with the code for the PNAS plague growth paper.  Therefore, we
## install the version that was released with the PNAS paper:
install_github("davidearn/epigrowthfit",
               ref = "pnas",
               dependencies = TRUE,
               build_vignettes = TRUE)			
