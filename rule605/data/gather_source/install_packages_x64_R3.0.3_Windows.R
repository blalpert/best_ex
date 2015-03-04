#
#' ---
#' title: "install_packages file for x64 Windows with R3.0.3"
#' author: "Nate Rock, Nanex LLC"
#' date: "last changed March 3, 2015"
#' ---

# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.

### Needed to work with x64 R-Studio 3.0.3 on Windows as some of the default installs aren't available for R3.0.3 x64

### ---- devtools ----
# taken from docs on install here: 
# https://github.com/hadley/devtools

library(devtools)
build_github_devtools()
#### Restart R before continuing ####
install.packages("devtools.zip", repos = NULL)
### Remove the package after installation
unlink("devtools.zip")
library(devtools)

### --- Rcpp ---
### Have to download and re-build Rcpp v0.11.3 from source to build bigvis
### http://stackoverflow.com/questions/28218175/error-in-installing-hadley-bigvis
### Note: this assumes you copied the rule605 directory to C:/
 
download.file("http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.11.3.tar.gz", "C:/rule605/Rcpp_0.11.3.tar.gz")
install.packages("C:/rule605/Rcpp_0.11.3.tar.gz", repos = NULL, type = "source")

### --- bigvis ---
devtools::install_github("hadley/bigvis")
library(bigvis)
