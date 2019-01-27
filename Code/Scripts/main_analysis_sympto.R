
source("Scripts/libraries.R")
source("Scripts/variables.R")
source("Scripts/variables_Sympto.R")
source("Scripts/functions.R")
source("Scripts/functions_Sympto.R")


# loading data

load(paste0(IO$sympto_data_000_original,"sy_abo.Rdata"), verbose = TRUE)
load(paste0(IO$sympto_data_000_original,"sy_cycleinfo.Rdata"), verbose = TRUE)
load(paste0(IO$sympto_data_000_original,"countries.Rdata"), verbose = TRUE)
load(paste0(IO$sympto_data_000_original,"sy_cycleday.Rdata"), verbose = TRUE)

#########################
# DATA CLEANING

users = clean_user_table(sy_abo)
cycles = clean_cycle_table(sy_cycleinfo)
cycledays = clean_cycleday_table(sy_cycleday)

save(users, file = paste0(IO$sympto_data_00_all_cleaned, "users.Rdata"))
save(cycles, file = paste0(IO$sympto_data_00_all_cleaned, "cycles.Rdata"))
save(cycledays, file = paste0(IO$sympto_data_00_all_cleaned, "cycledays.Rdata"))


#########################
# STATISTICS

load(file = paste0(IO$sympto_data_00_all_cleaned, "users.Rdata"), verbose = TRUE)
load( file = paste0(IO$sympto_data_00_all_cleaned, "cycles.Rdata"), verbose = TRUE)
load( file = paste0(IO$sympto_data_00_all_cleaned, "cycledays.Rdata"), verbose = TRUE)

source('data_processing_1_sympto.R')

# selecting "regular cycles"

ok.cycles = (!(cycles$is_first_cycle | cycles$is_last_cycle)) & 
  cycles$has_yellow_phase &
#  (cycles$n_obs >=5) &
  (!is.na(cycles$n_obs)) &
  (cycles$data_from_day_1) &
  (cycles$data_for_all_cycledays) &
  (cycles$has_bleeding) &
  (!cycles$susp_double_cycle) &
  (cycles$cycle_length>=10) & 
  (cycles$gap <= 15) &
  (cycles$gap_blue_pink <= 6) &
  ((cycles$bf == 0) | is.na(cycles$bf)) &
  (!cycles$susp_preg_birth)

ok.cycles[is.na(ok.cycles)] = FALSE

sum(ok.cycles)

cycles$is_valid_cycle = ok.cycles

source('data_processing_2_sympto.R')


# save processed tables

save(users, file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'users.Rdata'))
save(cycles, file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'cycles.Rdata'))
save(cycledays, file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'cycledays.Rdata'))


# load( file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'users.Rdata'), verbose  = TRUE)
# load( file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'cycles.Rdata'), verbose  = TRUE)
# load( file = paste0(IO$sympto_data_01_all_cycles_cleaned_and_processed,'cycledays.Rdata'), verbose  = TRUE)



# save tables with only 'valid' cycles

ok.cycles = cycles$is_valid_cycle

user_list = unique(cycles$user_id[ok.cycles])
m = match(users$user_id, user_list)
users$with_valid_cycles = FALSE
users$with_valid_cycles[!is.na(m)] = TRUE

cycle_list = unique(cycles$cycle_id[ok.cycles])
m = match(cycledays$cycle_id, cycle_list)
cycledays$is_valid_cycle = FALSE
cycledays$is_valid_cycle[!is.na(m)] = TRUE
cycledays.full = cycledays
cycledays = cycledays.full[!is.na(m),]


cycles.full = cycles
cycles = cycles[ok.cycles,]


save(users, file = paste0(IO$sympto_data_02_standard_cycles,'users.Rdata'))
save(cycles, file = paste0(IO$sympto_data_02_standard_cycles,'cycles.Rdata'))
save(cycles.full, file = paste0(IO$sympto_data_02_standard_cycles,'cycles_full.Rdata'))
save(cycledays, file = paste0(IO$sympto_data_02_standard_cycles,'cycledays.Rdata'))
save(cycledays.full, file = paste0(IO$sympto_data_02_standard_cycles,'cycledays_full.Rdata'))


load = FALSE

if(load){
load( file = paste0(IO$sympto_data_02_standard_cycles,'users.Rdata'), verbose = TRUE)
load( file = paste0(IO$sympto_data_02_standard_cycles,'cycles.Rdata'), verbose = TRUE)
load( file = paste0(IO$sympto_data_02_standard_cycles,'cycles_full.Rdata'), verbose = TRUE)
load( file = paste0(IO$sympto_data_02_standard_cycles,'cycledays.Rdata'), verbose = TRUE)
load( file = paste0(IO$sympto_data_02_standard_cycles,'cycledays_full.Rdata'), verbose = TRUE)
}



#########################
# MODELLING

cycle_ids = unique(cycles$cycle_id)

selected_cycle_ids = cycle_ids
#selected_cycle_ids = cycle_ids[1:10]


#### HMM
system.time({
for (cycle_id in selected_cycle_ids) {
  #cat(cycle_id, '\n')
  j = which(cycledays$cycle_id == cycle_id)
  cycletable = cycledays[j, ]
  
  res = try(most_likely_day_of_ovulation_hmm(cycletable = cycletable,
                                         debug = FALSE, no.print = TRUE),
            silent = TRUE)
  
  if(typeof(res)!= 'character'){res$cycle_id = cycle_id}
  
  save(res, file = paste0(IO$res_folder, cycle_id,'_',gsub(':','-',now()),'.Rdata'))
}
})

#COLLECT FITS RESULTS TOGETHER

source('collect_hmm_results_sympto.R')
cat(as.character(now()),'\n')

#########################
# RESULTS ANALYSIS

### define cycles for which ovu estimation is trustable

vv = (cycles$ovu.sd <= 1.5) &  # low standard deviation on ovulation estimation
  (cycles$D.T >= 0.15) & # sufficient temperature rise
  (cycles$confidence >= 0.75)

sum(vv, na.rm = TRUE)
v = which(vv)

cycles$reliable.ovu.est  = vv

# luteal phase duration

cycles$luteal.duration = cycles$cycle_length - cycles$ovu

# counting days from ovulation day in cycleday

m = match(cycledays$cycle_id, cycles$cycle_id)
ovuday.cycleday = round(cycles$ovu[m]) 
cycleday_from_ovu = cycledays$out_cycleday - ovuday.cycleday
cycledays$cycleday_from_ovu = cycleday_from_ovu
rm(m,ovuday.cycleday ,cycleday_from_ovu)

# normalized temperatures

m = match(cycledays$cycle_id, cycles$cycle_id)
low_norm_temp = cycledays$temp_c - cycles$low.T[m]
high_norm_temp = cycledays$temp_c - (cycles$low.T[m]+cycles$D.T[m])
low_norm_temp[cycles$low.T[m] == 0] = NA
high_norm_temp[cycles$low.T[m] == 0] = NA
cycledays$low_norm_temp = low_norm_temp
cycledays$high_norm_temp = high_norm_temp
rm(m,low_norm_temp, high_norm_temp)



### Saving results

save(users, file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'users.Rdata'))
save(cycles, file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'cycles.Rdata'))
save(cycledays, file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'cycledays.Rdata'))

if(load){
  load(file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'users.Rdata'), verbose = TRUE)
  load(file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'cycles.Rdata'), verbose = TRUE)
  load(file = paste0(IO$sympto_data_02_standard_cycles_with_HMM_res,'cycledays.Rdata'), verbose = TRUE)
}







