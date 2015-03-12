#
#' ----------
#' title:  "write_functions file"
#' author: "Bill Alpert, Barron's, 1.212.416.2742, william.alpert@barrons.com"
#' date:   "last changed March 12, 2015"
#' ----------

# Changes on 12MAR2015

# Commented out the commands to store results in directories "\net_pi" and "\e_q"
# which users would need to create on their drives. These stored results are useful only
# when comparing firms to one another, over separate runs of these scripts using the
# Rule 605 data from separate firms.


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
inst_pkgs = load_pkgs = c("plyr", "dplyr", "reshape2")

# Check to see if the packages are already installed
inst_pkgs = inst_pkgs[!(inst_pkgs %in%
                          installed.packages()[, "Package"])]

# install any missing packages
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load required pacakges
pkgs_loaded = lapply(load_pkgs, require, character.only = T)





# set the 'results_data' directory

results_data_dir <- "./analysis/results_data"

setwd(results_data_dir)


# --------------------
# All cleaned Form 605 data, write file
#
# --------------------

# 'MC_name' defined in 'form605_merge_data.R' line 40:

all_data_name <- paste(MC_name, "f605_mktble_df", "csv", sep = ".")
write.table(f605_mktble_df, all_data_name, row.names = TRUE, col.names = NA, sep = ",")

# --------------------
#
# Net price-improvement, write files
#
# --------------------


# make a data frame table of the average net price improvement measures of market orders

net_pi_mcaway_morder_measrs_dftemp <- data.frame(c( net_pi_mcaway_morder_wmean, net_pi_mcaway_morder_sp500_wmean, net_pi_mcaway_morder_notsp500_mean, net_pi_mcaway_morder_wmed, net_pi_mcaway_morder_sp500_wmed))

net_pi_mcaway_measrs_df <- reshape2::melt(net_pi_mcaway_morder_measrs_dftemp)
net_pi_mcaway_measrs_df <- dplyr::mutate(net_pi_mcaway_measrs_df, MarketCenter = MC_name)
write.table(net_pi_mcaway_measrs_df, "net_pi_mcaway_measrs_df.csv", row.names = TRUE, col.names = NA, sep = ",")


# make a data frame table of the volume weighted-quantiles of net price improvement on market orders, all months...by order size

# 'df_sz_all_w' defined in 'form605_analysis.R' file:

net_pi_mcaway_qntls_size_df <- data.frame(df_sz_all_w)
net_pi_mcaway_qntls_size_df <- mutate(net_pi_mcaway_qntls_size_df, MarketCenter = MC_name)
write.table(net_pi_mcaway_qntls_size_df, "net_pi_mcaway_qntls_size_df.csv", row.names = TRUE, col.names = NA, sep = ",")

# make a data frame table of the volume weighted-means of net price improvement on market orders, all months...by order size
net_pi_mcaway_wmean_size_df <- data.frame(df_sz_all_wmean)
net_pi_mcaway_wmean_size_df <- mutate(net_pi_mcaway_wmean_size_df, MarketCenter = MC_name)
write.table(net_pi_mcaway_wmean_size_df, "net_pi_mcaway_wmean_size_df.csv", row.names = TRUE, col.names = NA, sep = ",")


# ----------
#
# Effective-over-quoted spread write files
#
# ----------


effec_over_quoted_measrs_dftemp <- data.frame(c(effective_over_quoted_morder_mcaway_wmean, effective_over_quoted_morder_sp500_mcaway_wmean, effective_over_quoted_morder_notsp500_mcaway_wmean, effective_over_quoted_morder_mcaway_wmed, effective_over_quoted_morder_sp500_mcaway_wmed, effective_over_quoted_morder_notsp500_mcaway_wmed))

effec_over_quoted_measrs_df <- reshape2::melt(effec_over_quoted_measrs_dftemp)

effec_over_quoted_measrs_df <- dplyr::mutate(effec_over_quoted_measrs_df, MarketCenter = MC_name)
write.table(effec_over_quoted_measrs_df, "effec_over_quoted_measrs_df.csv", row.names = TRUE, col.names = NA, sep = ",")



# make a data frame table of the volume weighted-quantiles of effective-over-quoted on market orders, all months...by order size

eoq_mcaway_qntls_size_df <- data.frame(df_eoq_sz_all_w)
eoq_mcaway_qntls_size_df <- mutate(eoq_mcaway_qntls_size_df, MarketCenter = MC_name)
write.table(eoq_mcaway_qntls_size_df, "eoq_mcaway_qntls_size_df.csv", row.names = TRUE, col.names = NA, sep = ",")

# make a data frame table of the volume weighted-means of effective-over-quoted on market orders, all months...by order size

eoq_mcaway_wmean_size_df <- data.frame(df_eoq_sz_all_wmean)
eoq_mcaway_wmean_size_df <- mutate(eoq_mcaway_wmean_size_df, MarketCenter = MC_name)
write.table(eoq_mcaway_wmean_size_df, "eoq_mcaway_wmean_size_df.csv", row.names = TRUE, col.names = NA, sep = ",")

# ##########
#
# Save files for comparision between market centers and brokers
#
# ##########


# # save net-pi and effective-over-quoted measures for later use in brokers' Rule 606 analysis
# 
# net_pi_filename <- paste0("net_pi_wmed_sp500_", MC_name)
# 
# # net_pi_mcaway_morder_wmed_dftemp <- data.frame(c(net_pi_mcaway_morder_sp500_wmed))
# #
# # net_pi_mcaway_wmed_df <- reshape2::melt(net_pi_mcaway_wmed_dftemp)
# # net_pi_mcaway_wmed_df <- dplyr::mutate(net_pi_mcaway_wmed_df, MarketCenter = MC_name)
# # write.table(net_pi_mcaway_wmed_df, "net_pi_mcaway_wmed_df.csv", row.names = TRUE, col.names = NA, sep = ",")
# #
# 
# os_name = Sys.info()[['sysname']]
# if(os_name == "Windows"){
#   setwd("C:/net_pi")
# }else if(os_name =="Darwin"){  # OS X
#   setwd("~/net_pi")
# }
# 
# write.table(net_pi_mcaway_measrs_df, net_pi_filename, row.names = TRUE, col.names = NA, sep = ",")
# 
# # now the e_q file
# 
# e_q_filename <- paste0("e_q_", MC_name)
# 
# os_name = Sys.info()[['sysname']]
# if(os_name == "Windows"){
#   setwd("C:/e_q")
# }else if(os_name =="Darwin"){  # OS X
#   setwd("~/e_q")
# }
# 
# write.table(effec_over_quoted_measrs_df, e_q_filename, row.names = TRUE, col.names = NA, sep = ",")


# reset the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

#end
