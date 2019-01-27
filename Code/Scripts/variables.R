###### INIT
# variable lists initialisation

IO = list()
viz = list()
viz_cols = list()
par = list()
dict = list()

###### IO

# INPUTS
# path to data remains protected
source("../../FAM-Restricted-Access-Repo/Scripts/variables_IO.R")

# Sympto
IO$sympto_data = paste0(IO$restricted_data_folder,"Sympto/")
IO$sympto_data_01_all_cycles_cleaned_and_processed = paste0(IO$sympto_data,"02 processed/")
IO$sympto_data_02_standard_cycles = paste0(IO$sympto_data,"03 ST validated/")
IO$sympto_data_02_standard_cycles_with_HMM_res = paste0(IO$sympto_data,"04 HMM/")
IO$sympto_data_03_reliable_ovulation_estimation = paste0(IO$sympto_data,"05 reliable ovulation estimation/")


# Kindara
IO$kindara_data = paste0(IO$restricted_data_folder,"Kindara/")
IO$kindara_data_accounts = paste0(IO$kindara_data,"Accounts/")
IO$kindara_data_cycles = paste0(IO$kindara_data,"Cycles/")
IO$kindara_data_days = paste0(IO$kindara_data, "Days/")

# OUTPUTS
IO$output_data = "../Data/"
IO$output_Rdata = paste0(IO$output_data,"RData/")
IO$output_csv = paste0(IO$output_data,"csv aggregated data/")

IO$restricted_figure_data = paste0(IO$restricted_data_folder,"For_figures/")

IO$panels = "../Figures Tables Media/Figures/panels/"


###### MISC

par$date.format = "%Y-%m-%d %H:%M:%S"

viz$pdf.full.width = 16

viz$margin.temp = 0.08

#######################
# COLORS

#
viz_cols$colors_lsy = c(
  rgb(153,188,226, maxColorValue = 255),
  rgb(255,154,132, maxColorValue = 255),
  rgb(138,214,228, maxColorValue = 255),
  rgb(230,187,204, maxColorValue = 255),
  rgb(169,210,193, maxColorValue = 255),
  rgb(205,187,224, maxColorValue = 255),
  rgb(204,230,191, maxColorValue = 255),
  rgb(216,205,172, maxColorValue = 255),
  rgb(189,219,238, maxColorValue = 255),
  rgb(166,186,194, maxColorValue = 255))


