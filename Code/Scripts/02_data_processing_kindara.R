##### ORIGINAL ACCOUNTS data

# loading "accounts"
load(paste0(IO$kindara_data_accounts, "cleaned_accounts_onefile.Rdata"))


# users age now & when they registered

accounts$birth_day[year(accounts$birth_day) < 1940] = NA
accounts$birth_day[year(accounts$birth_day) > 2009] = NA

accounts$oldest_day[year(accounts$oldest_day) < 2012] = NA
accounts$oldest_day[year(accounts$oldest_day) > 2018] = NA

accounts$age_now = 2018 - year(accounts$birth_day)
accounts$age_registration = year(accounts$oldest_day) - year(accounts$birth_day)

save(accounts, file = paste0(IO$kindara_data_accounts, "processed_cleaned_accounts_onefile.Rdata"))



###### PROCESSING DAYS, CYCLES & USERS 
# CREATING NEW CYCLES TABLE FROM CYCLEDAYS


days.file.list = list.files(paste0(IO$kindara_data_days,"01 cleaned/"))
#days.file.list = days.file.list[1:2]

cat(as.character(now()),'\n')

system.time({
for(days.file in days.file.list){
  
  cat(days.file, '\n')
  
  load(paste0(days.folder.cleaned,days.file))
  
  # order days by user_id, cycle_number and cycledays
  
  o = order(days$user_id, days$cycle_number, days$cycleday)
  days = days[o,]
  
  cycles = unique(days[,c('user_id','cycle_number','cycle_id','length','start','end')])
  users = aggregate(cycle_number ~ user_id, cycles, max)
  colnames(users) = c('user_id','n_cycles')
  
  
  # normalised temperature from lowest and highest in cycle
  agg = aggregate(temperature ~ cycle_id, days, quantile, 0.25)
  m = match(days$cycle_id, agg$cycle_id)
  days$temp_diff_from_p25 = days$temperature - agg$temperature[m]
    
  # tracking frequency
  agg = aggregate(cycleday ~ cycle_id, days, length)
  m = match(cycles$cycle_id, agg$cycle_id)
  cycles$n_obs = agg$cycleday[m]
  cycles$tracking_freq = cycles$n_obs / cycles$length
  rm(agg, m)
  
  # is first cycle
  cycles$is_first_cycle = (cycles$cycle_number == 1)
  
  # is last cycle
  m = match(cycles$user_id, users$user_id)
  cycles$is_last_cycle = (cycles$cycle_number == users$n_cycles[m])
  
  
  # last observation day
  agg.last.obs.day = aggregate(cycleday ~ cycle_id, days, max)
  m = match(cycles$cycle_id, agg.last.obs.day$cycle_id)
  cycles$last_obs_cycleday = agg.last.obs.day$cycleday[m]
  rm(agg.last.obs.day, m)
  
  # observation gap
  
  gap.from.end = cycles$length - cycles$last_obs_cycleday
  
  gap = diff(days$cycleday); gap = gap - 1
  gap = data.frame(gap = gap, cycle_id = days$cycle_id[1:length(gap)])
  agg.gap = aggregate(gap ~ cycle_id, gap, max)
  
  m = match(cycles$cycle_id, agg.gap$cycle_id)
  cycles$gap = pmax(agg.gap$gap[m], gap.from.end)
  rm(gap.from.end, gap, agg.gap,m)
  
  # tracking sexual behavior
  
  agg = aggregate(cycleday ~ cycle_id + sex, days[days$sex>0,], length)
  colnames(agg)[3] = 'n'
  agg.tot = aggregate(n ~ cycle_id, agg, sum)
  m = match(cycles$cycle_id, agg.tot$cycle_id)
  cycles$n_sex_tot = agg.tot$n[m]; cycles$n_sex_tot[is.na(cycles$n_sex_tot)] = 0
    
  agg$sex = dict$sex$short.names[match(agg$sex, dict$sex$index)]
  agg = reshape(agg, v.names = 'n', idvar = 'cycle_id', timevar = 'sex',direction = 'wide')
  colnames(agg) = gsub('\\.','_',colnames(agg))
  agg[is.na(agg)] = 0
  test = merge(cycles, agg, by = 'cycle_id',all = TRUE)
  o = order(test$user_id, test$cycle_number)
  cycles = test[o,]  
  rm(agg, agg.tot, m, o)
  
  cycles$n_prot_sex[is.na(cycles$n_prot_sex)] = 0
  cycles$n_unprot_sex[is.na(cycles$n_unprot_sex)] = 0
  cycles$n_withdrawal[is.na(cycles$n_withdrawal)] = 0
  cycles$n_insemination[is.na(cycles$n_insemination)] = 0
  
  
  # tracking pregnancy tests
  agg = aggregate(cycleday ~ cycle_id + preg_test, days[days$preg_test %in% c(1,2),], length)
  colnames(agg)[3] = 'n'
  agg.tot = aggregate(n ~ cycle_id, agg, sum)
  m = match(cycles$cycle_id, agg.tot$cycle_id)
  cycles$n_preg_test = agg.tot$n[m]; cycles$n_preg_test[is.na(cycles$n_preg_test)] = 0
  
  agg$preg_test = dict$preg.test$names[match(agg$preg_test, dict$preg.test$index)]
  agg = reshape(agg, v.names = 'n', idvar = 'cycle_id', timevar = 'preg_test',direction = 'wide')
  colnames(agg) = c('cycle_id','n_preg_test_pos','n_preg_test_neg')
  agg[is.na(agg)] = 0
  test = merge(cycles, agg, by = 'cycle_id',all = TRUE)
  o = order(test$user_id, test$cycle_number)
  cycles = test[o,]  
  rm(agg, agg.tot, m, o)
  
  cycles$n_preg_test_pos[is.na(cycles$n_preg_test_pos)] = 0
  cycles$n_preg_test_neg[is.na(cycles$n_preg_test_neg)] = 0
  
  # tracking ovulation tests
  agg = aggregate(cycleday ~ cycle_id + opk, days[days$opk %in% c(1,2),], length)
  colnames(agg)[3] = 'n'
  agg.tot = aggregate(n ~ cycle_id, agg, sum)
  m = match(cycles$cycle_id, agg.tot$cycle_id)
  cycles$n_opk = agg.tot$n[m]; cycles$n_opk[is.na(cycles$n_opk)] = 0
  
  agg$opk = dict$opk$names[match(agg$opk, dict$opk$index)]
  agg = reshape(agg, v.names = 'n', idvar = 'cycle_id', timevar = 'opk',direction = 'wide')
  colnames(agg) = c('cycle_id','n_opk_pos','n_opk_neg')
  agg[is.na(agg)] = 0
  test = merge(cycles, agg, by = 'cycle_id',all = TRUE)
  o = order(test$user_id, test$cycle_number)
  cycles = test[o,]  
  rm(agg, agg.tot, m, o)
  
  cycles$n_opk_pos[is.na(cycles$n_opk_pos)] = 0
  cycles$n_opk_neg[is.na(cycles$n_opk_neg)] = 0
  
  # peak day
  agg = aggregate(cycleday ~ cycle_id, days[days$peak_day,], max)
  m = match(cycles$cycle_id, agg$cycle_id)
  cycles$peak_day = agg$cycleday[m]
  rm(agg, m)
  
  # suspected double cycles
  
  w = (days$cycleday > 10) & (days$length >= 40) & (days$cycleday_from_end <= -15) & (days$bleeding > 0)
  agg.sum = aggregate(bleeding ~ cycle_id, days[w,], sum)
  agg.max = aggregate(bleeding ~ cycle_id, days[w,], max)
  colnames(agg.sum) = c('cycle_id','sum_bleeding')
  colnames(agg.max) = c('cycle_id','max_bleeding')
  agg = merge(agg.sum, agg.max, by ='cycle_id', all = TRUE)
  
  gap.bleeding = diff(diff(days$cycleday[which(w)]))
  gap.bleeding = data.frame(gap = gap.bleeding, cycle_id = days$cycle_id[which(w)[1:length(gap.bleeding)]])
  gap.bleeding$gap[gap.bleeding$gap < 0] = NA
  gap.bleeding = aggregate(gap ~ cycle_id, gap.bleeding, min)
  
  agg = merge(agg, gap.bleeding, by = 'cycle_id', all = TRUE)
  
  agg$suspected_double_cycle = (agg$sum_bleeding >= 5) & (agg$max_bleeding >=2) & (agg$gap == 0)
  agg = agg[which(agg$suspected_double_cycle),]
  m = match(cycles$cycle_id, agg$cycle_id)
  cycles$suspected_double_cycle = !is.na(m)
  
  
  # number of days with bleeding
  agg = aggregate(bleeding ~ cycle_id, days[days$bleeding > 0 ,], length)
  m = match(cycles$cycle_id, agg$cycle_id)
  cycles$n_days_bleeding = agg$bleeding[m]
  cycles$n_days_bleeding[is.na( cycles$n_days_bleeding)] = 0
  rm(agg, m)
  
  # number of FAM observations
  cond = (days$temperature > 0) | 
        (!is.na(days$mucus)) | 
        (days$cervix_openness > 0)|
        (days$cervix_firmness > 0)|
        (days$cervix_height > 0)|
        (days$vaginal_sensation > 0)
  agg = aggregate(cycleday ~ cycle_id, days[which(cond),], length)
  m = match(cycles$cycle_id, agg$cycle_id)
  cycles$n_FAM_obs = agg$cycleday[m]
  cycles$n_FAM_obs[is.na(cycles$n_FAM_obs)] = 0
  rm(agg, m)
  
  
  # guessing goals
  
  cycles$goal_txt = 'Unknown'
  cycles$goal_txt[cycles$n_sex_tot >= 1] = 'Accept what comes'
  cycles$goal_txt[cycles$n_prot_sex >=1] = 'Contraception'
  cycles$goal_txt[cycles$n_insemination >=1] = 'Conception'
  cycles$goal_txt[cycles$n_sex_tot == 0] = 'Observation'
  cycles$goal_txt[cycles$n_opk > 0] = 'Conception'
  
  cycles$goal = dict$goal$index[match(cycles$goal_txt, dict$goal$names)]
  
  m = match(days$cycle_id, cycles$cycle_id)
  days$goal_txt = cycles$goal_txt[m]
  

  # first and last day of observation per users
  agg = aggregate(date ~ user_id, days, min)
  m = match(users$user_id, agg$user_id)
  users$first_obs = agg$date[m]
  agg = aggregate(date ~ user_id, days, max)
  m = match(users$user_id, agg$user_id)
  users$last_obs = agg$date[m]
  rm(agg, m)
  users$obs_duration_days = as.numeric(users$last_obs - users$first_obs)
  
  
  save(days, file = paste0(IO$kindara_data_days,"02 processed/",days.file))
  
  file.number = substr(days.file, 4,nchar(days.file))
  
  save(cycles,file =  paste0(IO$kindara_data_cycles,"02 processed/",'cycles',file.number))
  save(users,file =  paste0(IO$kindara_data_accounts,"02 processed/",'users',file.number))
  
}
})

