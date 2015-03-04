#
#' ----------
#' title:  "form605_makefile"
#' author: "Bill Alpert, Barron's, 1.212.416.2742, william.alpert@barrons.com"
#' date:   "last changed February 27, 2015"
#' ----------


# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.

# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  # create some directories that aren't in the default repo 
  dir.create("C:/rule605")
  dir.create("C:/net_pi")
  dir.create("C:/e_q")
  dir.create("C:/rule605/analysis/results_data", recursive=TRUE)
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  # create some directories that aren't in the default repo 
  dir.create("~/rule605")
  dir.create("~/net_pi")
  dir.create("~/e_q")
  dir.create("~/rule605/analysis/results_data", recursive=TRUE)  
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

# Any package required by the script below is given here
inst_pkgs = load_pkgs = c("rmarkdown", "knitr")

# Check to see if the packages are already installed
inst_pkgs = inst_pkgs[!(inst_pkgs %in%
                          installed.packages()[, "Package"])]

# install any missing packages
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load required packages
pkgs_loaded = lapply(load_pkgs, require, character.only = T)

# ---------------
# Merge and clean the raw Form 605 data
# ---------------
source("./data/gather_source/form605_merge_data.R")

# ---------------
# Analyze the Form 605 data
# ----------------

# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

source("./analysis/form605_analysis.R")

# ----------
#
# Render the 'Rule605_report" html document
#
# ----------

rmarkdown::render("./analysis/Rule605_report.Rmd", "html_document")

# ----------
#
# Save the analysis in data frame tables and Excel files
#
# ----------

# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

source("./analysis/form605_write_functions.R")

# end
