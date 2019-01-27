
# data cleaning

clean_user_table = function(sy_abo){
  
  users = sy_abo
  
  # clean column names
  colnames.table = colnames(sy_abo)
  colnames.table = gsub('su_','',colnames.table)
  
  # colnames.table[colnames.table == 'id'] = 'user_id'
  
  colnames(users) = colnames.table
  rm(colnames.table)
  
  
  users$creation_date = strptime(users$creation, format = par$date.format)
  users$last_connection = strptime(users$lst_con, format = par$date.format)
  users$last_sync = strptime(users$last_sync, format = par$date.format)
  users$modif_settings = strptime(users$modif_settings, format = par$date.format)
  
  users = users[,-which(colnames(users) =='creation')]
  users = users[,-which(colnames(users) =='lst_con')]
  
  users$year[users$year < 1900] = NA
  
  
  #expand features
  #users$country = countries$ct_name[match(users$country_id,countries$ct_id)]
  users$type_txt = dict$types$types.en[match(users$type, dict$types$index)]
  users$kg[users$kg == 0] = NA
  users$cm[users$cm == 0] = NA
  users$bmi = users$kg / (users$cm/100)^2
  users$bmi[users$bmi > 90] = NA
  users$menarche_year[users$menarche_year == 0] = NA
  
  o = order(users$user_id)
  users = users[o,]
  
  return(users)
  
}


# cleaning cycleinfo table

clean_cycle_table = function(sy_cycleinfo){
  
  cycles = sy_cycleinfo
  
  # clean column names
  colnames.table = colnames(cycles)
  colnames.table = gsub('ci_','',colnames.table)
  
  # colnames.table[colnames.table == 'abo_id'] = 'user_id'
  colnames.table[colnames.table == 'id'] = 'key'
  
  
  colnames(cycles) = colnames.table
  rm(colnames.table)
  
  #expand features
  
  cycles$first_day_of_cycle = as.Date("2000-01-01") + cycles$first_day_pos # - 1 
  cycles$cycle_id = paste0(cycles$user_id,'_',cycles$cycle_nb)
  cycles$goal_txt = dict$goals$goals[match(cycles$goal, dict$goals$index)]
  cycles$temp_method_txt =  temp_method.dict$temp_method[match(cycles$temp_method, temp_method.dict$index)]
  cycles$method = billings.dict$billings.short[match(cycles$billings, billings.dict$index)]
  cycles$ref = as.logical(-cycles$ref+2)
  
  o = order(cycles$user_id, cycles$cycle_nb)
  cycles = cycles[o,]
  
  return(cycles)
  
}



# cleaning cycleday table

clean_cycleday_table = function(sy_cycleday){
  
  cycledays = sy_cycleday
  
  # clean column names
  colnames.table = colnames(cycledays)
  colnames.table = gsub('cy_','',colnames.table)
  
  # colnames.table[colnames.table == 'abo_id'] = 'user_id'
  colnames.table[colnames.table == 'id'] = 'key'
  
  colnames(cycledays) = colnames.table
  rm(colnames.table)
  
  # remove the "exp" columns
  exp = grep('exp_', colnames(cycledays))
  cycledays = cycledays[,-exp]
  
  
  #expand features
  
  cycledays$obs_day = as.Date("2000-01-01") + (cycledays$day ) # - 1)
  #cycledays$input_time = strptime(cycledays$input_time, format = date.format)
  
  cycledays$temp_c_as_is = 35.8 + cycledays$temp * 0.05
  cycledays$temp_c = cycledays$temp_c_as_is
  cycledays$temp_c[cycledays$temp_c<=35.8] = NA
  
  cycledays$baseline = 35.8 + cycledays$out_baseline * 0.05
  
  cycledays$time_temp = cycledays$time * 0.5
  cycledays$time_temp[cycledays$time == 0] = NA
  
  cycledays$cycle_id = paste0(cycledays$user_id,'_',cycledays$cycle_nb)
  
  cycledays$day_id = paste0(cycledays$cycle_id,'_',cycledays$out_cycleday)
  o = order(cycledays$user_id, cycledays$cycle_nb, cycledays$out_cycleday)
  cycledays = cycledays[o,]
  
  return(cycledays)
  
}
