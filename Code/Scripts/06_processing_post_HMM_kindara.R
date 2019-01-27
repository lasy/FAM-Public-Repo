

cat('06_processing_post_HMM.R start \n')

cycles.file.list = list.files(cycles.folder.HMM)
cycles.file.list = paste0(cycles.folder.HMM, cycles.file.list )
#cycles.file.list = cycles.file.list[1:2]



tic()
for(cycles.file in cycles.file.list){
  cat(as.character(now()),'\n')
  cat(cycles.file, '\n')
  load(cycles.file)
  cat(dim(cycles),'\n')
  
  file.number = substr(cycles.file, start = 27, stop = nchar(cycles.file))
  load(paste0(days.folder.HMM, 'day',file.number))
  load(paste0(users.folder.HMM,'users',file.number))
  
  
  # CYCLES
  
  vv = (cycles$ovu.sd <= 1.5) &  # low standard deviation on ovulation estimation
    (cycles$D.T >= 0.15/5*9) & # sufficient temperature rise
    (cycles$confidence >= 0.75)
  
  sum(vv, na.rm = TRUE)
  v = which(vv)
  
  
  cycles$reliable.ovu.est  = vv
  
  
  # luteal phase duration
  
  cycles$luteal.duration = cycles$length - cycles$ovu
  
  
  # DAYS

  # counting days from ovulation day in cycleday
  
  m = match(days$cycle_id, cycles$cycle_id)
  ovuday.cycleday = round(cycles$ovu[m]) 
  cycleday_from_ovu = days$cycleday - ovuday.cycleday
  days$cycleday_from_ovu = cycleday_from_ovu
  rm(m,ovuday.cycleday ,cycleday_from_ovu)
  
  # normalized temperatures
  
  m = match(days$cycle_id, cycles$cycle_id)
  low_norm_temp = days$temperature - cycles$low.T[m]
  high_norm_temp = days$temperature - (cycles$low.T[m]+cycles$D.T[m])
  low_norm_temp[cycles$low.T[m] == 0] = NA
  high_norm_temp[cycles$low.T[m] == 0] = NA
  days$low_norm_temp = low_norm_temp
  days$high_norm_temp = high_norm_temp
  rm(m,low_norm_temp, high_norm_temp)
  
  
  # USERS
  
  agg = aggregate(ovu ~ user_id, cycles[v,], median)
  m = match(users$user_id, agg$user_id)
  users$ovu.med = agg$ovu[m]
  
  agg = aggregate(ovu ~ user_id, cycles[v,], mean)
  m = match(users$user_id, agg$user_id)
  users$ovu.avg = agg$ovu[m]
  
  agg = aggregate(ovu ~ user_id, cycles[v,], min)
  m = match(users$user_id, agg$user_id)
  users$ovu.min = agg$ovu[m]
  
  agg = aggregate(ovu ~ user_id, cycles[v,], max)
  m = match(users$user_id, agg$user_id)
  users$ovu.max = agg$ovu[m]
  
  agg = aggregate(ovu ~ user_id, cycles[v,], sd)
  m = match(users$user_id, agg$user_id)
  users$ovu.sd = agg$ovu[m]
  
  agg = aggregate(reliable.ovu.est ~ user_id, cycles[v,], sum)
  m = match(users$user_id, agg$user_id)
  users$n_cycles.ovu_est = agg$reliable.ovu.est[m]
  
  
  j = which(cycles$reliable.ovu.est & (cycles$ovu >= 19))
  agg = aggregate(reliable.ovu.est ~ user_id, cycles[j,], sum)
  m = match(users$user_id, agg$user_id)
  users$n_cycles.late_ovu = agg$reliable.ovu.est[m]
  k = which(!is.na(users$n_cycles.ovu_est) & is.na(users$n_cycles.late_ovu))
  users$n_cycles.late_ovu[k] = 0
  
  users$fraction_cycles.late_ovu = users$n_cycles.late_ovu/users$n_cycles.ovu_est 
  
  
  ### SAVE
  
  save(days, file = paste0(days.folder.postHMM, 'day',file.number))
  save(cycles, file = paste0(cycles.folder.postHMM, 'cycles',file.number))
  save(users, file = paste0(users.folder.postHMM, 'users',file.number))
  
}
toc()


cat('06_processing_post_HMM.R end \n')

