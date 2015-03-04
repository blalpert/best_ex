#
#' ---
#' title: "install_packages file for x64 Windows with R3.1.2"
#' author: "Nate Rock, Nanex LLC"
#' date: "last changed March 3, 2015"
#' ---

# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.

### Needed to work with x64 R-Studio 3.1.3 on Windows as some of the default installs aren't available for R3.1.2 x64

### ---- devtools ----
install.packages("devtools")
library(devtools)

### ---- RTools3.2 ---
### http://cran.r-project.org/bin/windows/Rtools/
### *NOTE* When installing, the default path worked but make sure to have it set the PATH environment variables

### --- Rcpp ---
### Have to download and re-build Rcpp v0.11.3 from source to build bigvis
### http://stackoverflow.com/questions/28218175/error-in-installing-hadley-bigvis
### Note: this assumes you copied the rule605 directory to C:/
remove.packages("Rcpp")
download.file("http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.11.3.tar.gz", "C:/rule605/Rcpp_0.11.3.tar.gz")
install.packages("C:/rule605/Rcpp_0.11.3.tar.gz", repos = NULL, type = "source")

### --- bigvis ---
devtools::install_github("hadley/bigvis")
library(bigvis)
