source("libraries.R")
source("functions.R")
source("functions_Kindara.R")
source("variables.R")
source("variables_Kindara.R")


###############
# DATA CLEANING

source('01_data_cleaning_kindara.R')



###############
# DATA PROCESSING - SIMPLE STATS

source('02_data_processing_kindara.R') 


###############
# CYCLE SELECTION

source('03_data_selection_for_HMM_kindara.R')


#########################
# HMM

source('04_run_HMM_kindara.R.R')


#########################
# COLLECTING HMM RESULTS

source('05_collect_HMM_results_kindara.R')


#########################
# PROCESSING POST HMM 

source('06_processing_post_HMM_kindara.R')
