
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

source('data_processing_1.R')

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

source('data_processing_2.R')


# save processed tables

save(users, file = '../Data/02 processed/users.Rdata')
save(cycles, file = '../Data/02 processed/cycles.Rdata')
save(cycledays, file = '../Data/02 processed/cycledays.Rdata')


#load(file = '../Data/02 processed/users.Rdata')
#load(file = '../Data/02 processed/cycles.Rdata')
#load(file = '../Data/02 processed/cycledays.Rdata')


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

save(users, file = '../Data/03 ST validated/users.Rdata')
save(cycles, file = '../Data/03 ST validated/cycles.Rdata')
save(cycles.full, file = '../Data/03 ST validated/cycles_full.Rdata')
save(cycledays, file = '../Data/03 ST validated/cycledays.Rdata')
save(cycledays.full, file = '../Data/03 ST validated/cycledays_full.Rdata')


load = FALSE

if(load){
load(file = '../Data/03 ST validated/users.Rdata')
load(file = '../Data/03 ST validated/cycles.Rdata')
load(file = '../Data/03 ST validated/cycles_full.Rdata')
load(file = '../Data/03 ST validated/cycledays.Rdata')
load(file = '../Data/03 ST validated/cycledays_full.Rdata')
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
  
  save(res, file = paste0(res_folder, cycle_id,'_',gsub(':','-',now()),'.Rdata'))
}
})

#COLLECT FITS RESULTS TOGETHER

source('collect_hmm_results.R')
cat(as.character(now()),'\n')

#########################
# RESULTS ANALYSIS


### TO DO confidence index
### XXX


### TO DO define cycles for which ovu estimation is trustable

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

save(users, file = '../Data/04 HMM/users.Rdata')
save(cycles, file = '../Data/04 HMM/cycles.Rdata')
save(cycledays, file = '../Data/04 HMM/cycledays.Rdata')



if(load){
  load(file = '../Data/04 HMM/users.Rdata')
  load(file = '../Data/04 HMM/cycles.Rdata')
  load(file = '../Data/04 HMM/cycledays.Rdata')
}














# Table S1

# full data set

load(file = '../Data/01 cleaned/users.Rdata')
load(file = '../Data/01 cleaned/cycles.Rdata')
load(file = '../Data/01 cleaned/cycledays.Rdata')

nrow(users)
length(unique(cycledays$cycle_id))
sum(cycledays$input_source > 0)

# standard cycles

load(file = '../Data/03 ST validated/users.Rdata')
load(file = '../Data/03 ST validated/cycles.Rdata')
load(file = '../Data/03 ST validated/cycledays.Rdata')

sum(users$with_valid_cycles)
length(unique(cycledays$cycle_id))
sum(cycledays$input_source > 0)


# reliable ovulation

load(file = '../Data/04 HMM/users.Rdata')
load(file = '../Data/04 HMM/cycles.Rdata')
load(file = '../Data/04 HMM/cycledays.Rdata')


m = match(cycledays$cycle_id, cycles$cycle_id[cycles$reliable.ovu.est])
cycledays$reliable.ovu.est = !is.na(m)

length(unique(cycles$user_id[cycles$reliable.ovu.est]))
sum(cycles$reliable.ovu.est)
sum(cycledays$input_source[cycledays$reliable.ovu.est] >0)


# n users with at least 3 cycles that have a reliable ovulation




do = FALSE

if(do){



# normal cycles
j.norm = (cd.select.with.fit.results$err <0.2) & (cd.select.with.fit.results$ovu.hmm.all == 14) & (cd.select.with.fit.results$cycle_length == 28)
sum(j.norm, na.rm = TRUE)


# PCOS cycles
j.pcos = (cd.select.with.fit.results$err <0.2) & (cd.select.with.fit.results$ovu.hmm.all %in% 22:25) & (cd.select.with.fit.results$cycle_length %in% 29:32)
sum(j.pcos, na.rm = TRUE)

# short luteal phase
j.lut.d = (cd.select.with.fit.results$err <0.2) & (cd.select.with.fit.results$ovu.hmm.all %in% 22:25) & (cd.select.with.fit.results$cycle_length == 28)
sum(j.lut.d, na.rm = TRUE)

# temperature drop
j.drop = (cd.select.with.fit.results$err <0.2) & 
  (cd.select.with.fit.results$ovu.hmm.all %in% 12:15) & 
  (cd.select.with.fit.results$cycle_length == 28) & 
  (cd.select.with.fit.results$prog.drop.day<=5) & 
  (cd.select.with.fit.results$Drop <=0.2)
sum(j.drop, na.rm = TRUE)

j.drop.ext = (cd.select.with.fit.results$err <0.2) & 
  (cd.select.with.fit.results$ovu.hmm.all %in% 12:15) & 
  (cd.select.with.fit.results$cycle_length == 28) & 
  (cd.select.with.fit.results$prog.drop.day<=8) & 
  (cd.select.with.fit.results$Drop <=0.4)
sum(j.drop, na.rm = TRUE)


# PCOS LONG cycles
j.pcos.l = (cd.select.with.fit.results$err <0.2) & (cd.select.with.fit.results$ovu.hmm.all %in% 32:35) & (cd.select.with.fit.results$cycle_length %in% 39:42)
sum(j.pcos.l, na.rm = TRUE)

# PCOS SUPER LONG cycles
j.pcos.sl = (cd.select.with.fit.results$err <0.2) & (cd.select.with.fit.results$ovu.hmm.all %in% 42:45) & (cd.select.with.fit.results$cycle_length %in% 49:52)
sum(j.pcos.sl, na.rm = TRUE)



#labels
labels = rep(NA, nrow(cd.select.with.fit.results))
labels[j.norm] = 'norm'
labels[j.pcos] = 'PCOS'
labels[j.lut.d] = 'short_lut'
labels[j.drop] = 'temp_drop'
labels[j.pcos.l] = 'PCOS_long'
labels[j.pcos.sl] = 'PCOS_superlong'


s = which(!is.na(labels))
s = which(j.pcos.sl)
m = match(cd.select.with.fit.results$cycle_id[s],selected_cycle_ids)
m = match(c('5180_3','78553_2'),selected_cycle_ids)
m = c(1:100)


#### PLOT THE FITS
for (cycle_id in selected_cycle_ids[m]) {
  cat(cycle_id, '\n')
  j = which(cycleday$cycle_id == cycle_id)
  cycletable = cycleday[j, ]
  k = which(cd.select.with.fit.results$cycle_id == cycle_id)
  
  filename = get_latest_fit_filename(cycle_id = cycle_id, global_path_Rdata = global_path_Rdata)
  load(filename)
  
  pdf.name = paste0(global_path_viz,'fits/', cycle_id,'_',labels[k],'_',gsub(':','-',now()), '.pdf')
  
  if (typeof(opt) != "character") {
    ### plot results
    pdf(pdf.name,
        width = 7,
        height =  5, useDingbats = FALSE)
    cat('here plot\n')
    plot_cycle_with_ovu_detection(cycletable = cycletable, opt = opt, publi = TRUE)
    dev.off()
  }else{
    pdf(pdf.name,
        width = 7,
        height =  5, useDingbats = FALSE)
    cat('here plot with fit error\n')
    par(mfrow = c(4, 1))
    plot_cycle(subtable = cycletable)
    plot(0, 0)
    dev.off()
  }
}  


}


}






