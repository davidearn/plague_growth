## install all packages needed by library.R
pkgs <- scan("required_packages", comment="#", what=character(0), quiet=TRUE)
i1 <- installed.packages()
pkgs <- pkgs[!pkgs %in% rownames(i1)]
install.packages(pkgs)
library(remotes)
install_github("bbolker/bbmle")
install_github("davidearn/epigrowthfit",dependencies=TRUE)




