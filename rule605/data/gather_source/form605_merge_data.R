#
#' ---
#' title: "form605_merge_data file"
#' author: "Bill Alpert, Barron's, 1.212.416.2742, william.alpert@barrons.com"
#' date: "last changed February 27, 2015"
#' ---


# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.


# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}


# Any package required by the script below is given here
inst_pkgs = load_pkgs = c("plyr", "dplyr")

# Check to see if the packages are already installed
inst_pkgs = inst_pkgs[!(inst_pkgs %in%
                          installed.packages()[, "Package"])]

# install any missing packages
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load required pacakges
pkgs_loaded = lapply(load_pkgs, require, character.only = T)


# -----------------------------
# load the raw Form 605 data from the text files

# For import, list the file names and the Form 605 fields that will be our column names

fnames_605  <- list.files(path = "./data/f605_data")

MC_name <- fnames_605[1]

form605_fields <- c("participant", "market_center", "date",
                    "ticker", "order_type", "order_size",
                    "total_orders", "total_shrs", "cancelled_shrs",
                    "mc_exec_shrs", "away_exec_shrs", "shrs_0to9sec",
                    "shrs_10to29sec", "shrs_30to59sec", "shrs_60to299sec",
                    "shrs_5to30min", "avg_realzd_spread", "avg_effec_spread",
                    "px_improved_shrs", "px_improved_avg_amt", "px_improved_avg_secs",
                    "at_quote_shrs", "at_quote_avg_secs", "outside_quote_shrs",
                    "outside_quote_avg_amt", "outside_quote_avg_sec")

# ---------------
# Import raw monthly data files, consolidate them into a 'data frame' table, for analysis


f605_data_dir <- "./data/f605_data"

setwd(f605_data_dir)

form_605_combined <- (lapply(fnames_605, read.table,
                             header= FALSE,
                             sep = "|",
                             fill = TRUE,
                             col.names = form605_fields,
                             colClasses = c("character", "character", "character",
                                            "character", "factor", "factor",
                                            "numeric", "numeric","numeric",
                                            "numeric","numeric","numeric",
                                            "numeric","numeric","numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "integer",
                                            "numeric", "numeric")))


form_605_all <- plyr::rbind.fill(form_605_combined)


## load names of exchange listings and s&p500 and russell1000 constituents,as of December 1, 2014

# change working directory from 'f605_data' to 'constituent_data'
constituent_data_dir <- "../constituent_data"

setwd(constituent_data_dir)

# read the files

sp500       <- read.table("sp500constituents.csv", header = FALSE)

russell1000      <- read.table("russell1000_constituents.csv", header = FALSE)

NYSE        <- read.table("tickers_NYSE.csv", header = FALSE)

NASDAQ      <- read.table("tickers_NASDAQ.csv", header = FALSE)

AMEX        <- read.table("tickers_AMEX.csv", header = FALSE)



## Add columns denoting listings and S&P 500 or Russell1000 membership

in_sp500 <- form_605_all[,4] %in% sp500[,1]

in_r1000 <- form_605_all[,4] %in% russell1000[,1]

on_nyse <- form_605_all[,4] %in% NYSE[,1]

on_nasdaq <- form_605_all[,4] %in% NASDAQ[,1]

on_amex <- form_605_all[,4] %in% AMEX[,1]

form_605_all <- cbind(form_605_all, in_sp500, in_r1000, on_nyse, on_nasdaq, on_amex)

# Filter to include only the rows for 'market orders' and "marketable limit orders'

form_605_mktble <- dplyr::filter(form_605_all, order_type == 11 | order_type ==  12)

# Filter to exclude rows where the market center executed no shares at all

form_605_mktble <- dplyr::filter(form_605_mktble,  mc_exec_shrs != 0)

# The name we'll use for the data frame:

f605_mktble_df <- form_605_mktble


# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

# end
