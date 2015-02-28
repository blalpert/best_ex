#
#' ---
#' title: "install_packages file"
#' author: "Bill Alpert, Barron's, 1.212.416.2742, william.alpert@barrons.com"
#' date: "last changed February 27, 2015"
#' ---


# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.

install.packages("devtools")
library(devtools)
devtools::install_github("hadley/bigvis")
library(bigvis)
