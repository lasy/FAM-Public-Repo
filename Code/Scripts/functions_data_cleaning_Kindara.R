

#ACCOUNT

clean_account_table = function(accounts, accounts.masterlist){
  
  
  # keep only the following columns: 
  # - kindara_id (rename: user_id)
  # - height
  # - birth_day
  # - objective
  # - last_stored
  # - oldest_day
  # - hidden
  
  accounts$user_id = accounts$kindara_id
  accounts = accounts[,c('user_id','height','birth_day','objective','last_stored','oldest_day','hidden')]
  
  # only keep accounts from masterlist
  ### PROBLEM: the accounts number do not match!!!
  
  #k = match(accounts$user_id, accounts.masterlist)
  #j = which(!is.na(k))
  #clean.accounts = accounts[j,]
  
  clean.accounts = accounts  
  
  # transform dates into actual date (oldest_day, birth_day) or datetime (last_stored)
  
  k = which(nchar(clean.accounts$last_stored) <= 0)
  clean.accounts$last_stored[k] = NA
  last_stored_dt = as_datetime(clean.accounts$last_stored)
  last_stored = as.Date(substr(clean.accounts$last_stored,start = 1,stop = 10))
  clean.accounts$last_stored_dt = last_stored_dt
  clean.accounts$last_stored = last_stored
  rm(last_stored_dt, last_stored)
  
  oldest_day = as.Date(clean.accounts$oldest_day)
  clean.accounts$oldest_day = oldest_day
  rm(oldest_day)
  
  k = which(nchar(clean.accounts$birth_day) <= 0)
  clean.accounts$birth_day[k] = NA
  birth_day = as.Date(clean.accounts$birth_day)
  clean.accounts$birth_day = birth_day
  rm(birth_day)
  rm(k)
  
  # remove duplicates
  
  k = which(duplicated(clean.accounts$user_id))
  if(length(k) > 0){clean.accounts = clean.accounts[-k,]}
  
  
  return(clean.accounts)
  
  
}


# CYCLES

clean_cycle_table = function(cycles, accounts.masterlist){
  
  # keep only the following columns:
  # - account (rename: user_id)
  # - start
  # - end
  # - name
  # - pregnancy
  # - period_length
  # - peak_day
  # - peak_day_auto
  # - temp_shift
  # - temp_shift_auto
  # - coverline
  # - coverline_auto
  # - avr_luteal_temp
  # - avr_follicular_temp
  # - custom
  
  cycles$user_id = cycles$account
  cycles = cycles[,c('user_id','start','end','pregnancy','period_length', #'name',
                     'peak_day','peak_day_auto','temp_shift','temp_shift_auto',
                     'coverline','coverline_auto','avr_luteal_temp','avr_follicular_temp','custom')]
  
  # transform dates into actual date (start, end, temp_shift,temp_shift_auto,peak_day, peak_day_auto) or datetime (updated, stored)
  
  k = which(nchar(cycles$start)==0)
  cycles$start[k] = NA
  start = as.Date(cycles$start)
  cycles$start = start
  rm(start)
  
  k = which(nchar(cycles$end)==0)
  cycles$end[k] = NA
  end = as.Date(cycles$end)
  cycles$end = end
  rm(end)
  
  k = which(nchar(cycles$peak_day)==0)
  cycles$peak_day[k] = NA
  peak_day = as.Date(cycles$peak_day)
  cycles$peak_day = peak_day
  rm(peak_day)
  
  k = which(nchar(cycles$peak_day_auto)==0)
  cycles$peak_day_auto[k] = NA
  peak_day_auto = as.Date(cycles$peak_day_auto)
  cycles$peak_day_auto = peak_day_auto
  rm(peak_day_auto)
  
  k = which(nchar(cycles$temp_shift)==0)
  cycles$temp_shift[k] = NA
  temp_shift = as.Date(cycles$temp_shift)
  cycles$temp_shift = temp_shift
  rm(temp_shift)
  
  k = which(nchar(cycles$temp_shift_auto)==0)
  cycles$temp_shift_auto[k] = NA
  temp_shift_auto = as.Date(cycles$temp_shift_auto)
  cycles$temp_shift_auto = temp_shift_auto
  rm(temp_shift_auto)
  
  # remove duplicates
  
  k = which(duplicated(cycles[,c('user_id','start')]))
  if(length(k) > 0){cycles = cycles[-k,]}
  
  
  # ADD - cycle number
  
  agg = aggregate(start ~ user_id, cycles, frank)
  m = match(cycles$user_id,agg$user_id)
  cycle_number = unlist(agg$start[unique(m)])
  #tmp = data.frame(cycles$user_id,cycles$start,cycles$end, cycle_number)
  rm(agg, m)
  
  cycles$cycle_number = cycle_number
  
  # ADD - cycle id (= user_id + cycle_number)
  cycle_id = paste0(cycles$user_id, '_', cycles$cycle_number)
  cycles$cycle_id = cycle_id
  
  rm(cycle_number, cycle_id)
  
  # ADD cycle length
  cycles_length = as.numeric(cycles$end - cycles$start + 1)
  cycles$cycle_length = cycles_length
  rm(cycles_length)
  
  # only keep accounts from masterlist
  
  k = match(cycles$user_id, accounts.masterlist)
  j = which(!is.na(k))
  clean.cycles = cycles[j,]
  rm(k, j)
  
  return(clean.cycles)
  
}



# DAYS

clean_day_table = function(days, accounts.masterlist){
  
  # DAY
  
  colnames(days)[colnames(days) == 'account'] = 'user_id'
  
  # transform dates into actual date (date) 
  k = which(nchar(days$date)==0)
  days$date[k] = NA
  date = as.Date(days$date)
  days$date = date
  rm(date)
  
  
  # transform times into numbers (temp_time)
  temp_time_str = substr(days$temp_time,12,19)
  temp_time = times(temp_time_str)
  days$temp_time = temp_time
  rm(temp_time, temp_time_str)
  
  
  # create just 1 variable for 
  # - bleeding (menstruation + spotting)
  # - cervical fluid (NA, 0, 1,2,3, 4,5,6, 7,8,9, 10,11,12) (NA, no_fluid,  fluid_sticky,fluid_creamy, fluid_eggwhite, fluid_watery)
  
  spotting = as.logical(days$spotting)
  days$spotting = spotting
  rm(spotting)
  
  bleeding = days$menstruation
  bleeding[(bleeding == 0) & (days$spotting)] = 0.5
  days$bleeding = bleeding
  rm(bleeding)
  
  
  no_fluid = as.logical(days$no_fluid)
  days$no_fluid = no_fluid
  rm(no_fluid)
  
  mucus = rep(0, nrow(days)) * NA
  mucus[days$no_fluid] = 0
  mucus[which(days$fluid_sticky>0)] = days$fluid_sticky[which(days$fluid_sticky>0)]
  mucus[which(days$fluid_creamy>0)] = days$fluid_creamy[which(days$fluid_creamy>0)] + 3
  mucus[which(days$fluid_eggwhite>0)] = days$fluid_eggwhite[which(days$fluid_eggwhite>0)] + 6
  mucus[which(days$fluid_watery>0)] = days$fluid_watery[which(days$fluid_watery>0)] + 9
  
  days$mucus = mucus
  rm(mucus)
  
  
  questionable_temp = as.logical(days$questionable_temp)
  days$questionable_temp = questionable_temp
  rm(questionable_temp)
  
  first_day = as.logical(days$first_day)
  days$first_day = first_day
  rm(first_day)
  
  peak_day = as.logical(days$peak_day)
  days$peak_day = peak_day
  rm(peak_day)
  
  
  conception = as.logical(days$conception)
  days$conception = conception
  rm(conception)
  
  
  
  
  
  
  # ADD - cycle_number
  
  # in the future, should be done based on the cycles tables
  # in the meanwhile, one needs to create a table similar to "cycles" but based on days
  
  cycles.d = days[days$first_day, c('user_id','date')]
  colnames(cycles.d) = c('user_id', 'start')
  
  agg = aggregate(start ~ user_id, cycles.d, frank)
  m = match(cycles.d$user_id,agg$user_id)
  cycle_number = unlist(agg$start[unique(m)])
  #tmp = data.frame(cycles$user_id,cycles$start,cycles$end, cycle_number)
  rm(agg, m)
  
  cycles.d$cycle_number = cycle_number
  rm(cycle_number)
  
  # add cycle_id
  
  cycle_id = paste0(cycles.d$user_id, '_', cycles.d$cycle_number)
  cycles.d$cycle_id = cycle_id
  rm(cycle_id)
  
  
  # add cycle end date
  cycles.d2 = cycles.d[order(cycles.d$user_id,cycles.d$cycle_number),]
  cycles.d = cycles.d2
  rm(cycles.d2)
  
  N = nrow(cycles.d)
  end = as.Date(rep(0,N) * NA, origin = '2000-01-01')
  end[1:(N-1)] = cycles.d$start[2:N]-1
  end[diff(cycles.d$cycle_number)<=0] = NA
  cycles.d$end = end
  cycles.d = cycles.d[,c(1,2,5,3,4)]
  rm(end)
  
  cyclelength = cycles.d$end - cycles.d$start + 1
  cycles.d$length = as.numeric(cyclelength)
  rm(cyclelength)
  
  # ADD cyclenumber & cycle_id to days
  
  days.t = days %>% 
    left_join(cycles.d[,c('user_id','cycle_id','start','end','cycle_number', 'length')], by = 'user_id') %>% 
    filter((date <= end) & (date >= start))
  
  days = days.t
  rm(days.t)
  
  # ADD cycleday
  
  cycleday = days$date - days$start + 1
  days$cycleday = as.numeric(cycleday)
  rm(cycleday)
  
  cycleday_from_end = days$cycleday - days$length - 1
  days$cycleday_from_end = cycleday_from_end
  rm(cycleday_from_end)
  
  
  
  
  # only keep accounts from masterlist
  
  # in the future, coordinate with the masterlist for real
  
  # k = match(days$user_id, accounts.masterlist)
  # j = which(!is.na(k))
  # clean.days = days[j,]
  # rm(k, j)
  
  
  # in the meanwhile, only keep the accounts that have at least 4 finished cycles
  
  agg = aggregate(cycle_number ~ user_id, cycles.d, max)
  j = which(agg$cycle_number > 4)
  long.term.users = agg$user_id[j]
  m = match(days$user_id, long.term.users)
  days = days[!is.na(m),]
  rm(agg,j,m, long.term.users)
  
  return(days)
  
}
