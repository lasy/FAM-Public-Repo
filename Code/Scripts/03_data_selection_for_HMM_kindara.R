
###### SELECTING "ok cycles" in tables DAYS, CYCLES & USERS 

cycles.folder.processed = paste0(IO$kindara_data_cycles,"02 processed/")
cycles.file.list = list.files(cycles.folder.processed)
#cycles.file.list = cycles.file.list[1:2]

users.folder.processed =  paste0(IO$kindara_data_accounts,"02 processed/")

days.folder.processed =  paste0(IO$kindara_data_days,"02 processed/")

cycles.folder.selected = paste0(IO$kindara_data_cycles,"03 processed/")
users.folder.selected =  paste0(IO$kindara_data_accounts,"03 processed/")
days.folder.selected =  paste0(IO$kindara_data_days,"03 processed/")


cat(as.character(now()),'\n')

system.time({
  for(cycles.file in cycles.file.list){
    
    cat(cycles.file, '\n')
    file.number = substr(cycles.file, 7,nchar(cycles.file))
    
    load(paste0(cycles.folder.processed,cycles.file))
    
    ok = (!cycles$is_first_cycle) & # we don't want the first cycles
      (!cycles$is_last_cycle) & # nor do we want the last cycles (which is likely unfinished)
      (cycles$n_preg_test_pos == 0) & # we don't want cycles with positive pregnancy test
      (!cycles$suspected_double_cycle) & # we don't want double cycles
      (cycles$gap <= 15) & # we don't want big gap in observation
      (cycles$n_obs >= 8) & # we want at least 8 observation per cycle
      ((cycles$length - cycles$n_days_bleeding) > 3) & # we don't want cycles that are only bleeding
      (cycles$n_FAM_obs > 0) # we want cycles that have at least one observation in temp, mucus, cervix or feel
    
    cycles$selected_for_hmm = ok
    
    # reflecting on users
    load(file = paste0(users.folder.processed,'users',file.number))
    agg = aggregate(selected_for_hmm ~ user_id, cycles, sum)
    m = match(users$user_id, agg$user_id)
    users$n_cycles_selected_for_hmm = agg$selected_for_hmm[m]
    
    
    ok.cycles = ok
    
    # average, median, min and max and sd cycle length
    agg = aggregate(length ~ user_id, cycles[ok.cycles,], mean)
    m = match(users$user_id, agg$user_id)
    users$cycle_length.avg = agg$length[m];rm(agg, m)
    agg = aggregate(length ~ user_id, cycles[ok.cycles,], median)
    m = match(users$user_id, agg$user_id)
    users$cycle_length.med = agg$length[m];rm(agg, m)
    agg = aggregate(length ~ user_id, cycles[ok.cycles,], min)
    m = match(users$user_id, agg$user_id)
    users$cycle_length.min = agg$length[m];rm(agg, m)
    agg = aggregate(length ~ user_id, cycles[ok.cycles,], max)
    m = match(users$user_id, agg$user_id)
    users$cycle_length.max = agg$length[m];rm(agg, m)
    agg = aggregate(length ~ user_id, cycles[ok.cycles,], sd)
    m = match(users$user_id, agg$user_id)
    users$cycle_length.sd = agg$length[m];rm(agg, m)
    
    
    # average & median number of observation
    agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], mean)
    m = match(users$user_id, agg$user_id)
    users$n_obs.avg = agg$n_obs[m];rm(agg, m)
    agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], median)
    m = match(users$user_id, agg$user_id)
    users$n_obs.med = agg$n_obs[m];rm(agg, m)
    agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], min)
    m = match(users$user_id, agg$user_id)
    users$n_obs.avg.min = agg$n_obs[m];rm(agg, m)
    agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], max)
    m = match(users$user_id, agg$user_id)
    users$n_obs.max = agg$n_obs[m];rm(agg, m)
    agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], sd)
    m = match(users$user_id, agg$user_id)
    users$n_obs.sd = agg$n_obs[m];rm(agg, m)
    

   
    
    # reflecting selection on days
    load(file = paste0(days.folder.processed,'day',file.number))
    m = match(days$cycle_id, cycles$cycle_id)
    days$selected_for_hmm = ok[m]
    days = days[days$selected_for_hmm,]
    
    # and only keeping the valid cycles in the cycles table
    cycles = cycles[ok,]
    

    save(days, file = paste0(days.folder.selected,'day',file.number))
    save(cycles,file =  paste0(cycles.folder.selected,'cycles',file.number))
    save(users,file =  paste0(users.folder.selected,'users',file.number))
  }
})

