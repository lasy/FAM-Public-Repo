
source('Scripts/functions_data_cleaning_Kindara.R')
source('Scripts/functions_HMM_Kindara.R')
source('Scripts/functions_for_plots_Kindara.R')




compute_weights = function(cycletable = cycletable,  debug = FALSE){
  
  time.points = cycletable$cycleday
  temp.data = cycletable$temperature
  n.days = length(time.points)
  
  ## weighting the residues 
  # by reliability of temperature (i.e. giving less weight to temperature that deviates from the median time)
  # by giving less weight to outliers = temperature diverging by more than delta t from their neighbours
  
  valid.day.temp = !is.na(temp.data)
  weights = rep(1, n.days)
  if(sum(valid.day.temp, na.rm = TRUE)<3){return(weights)}
  
  temp.time = cycletable$temp_time
  valid.day.temp.time = valid.day.temp & (!is.na(temp.time))
  med.time = median(temp.time[valid.day.temp.time], na.rm = TRUE)
  
  if(! (is.null(med.time) || is.na(med.time) )){
    #time
    l = log(0.95/0.05)
    corrected.time.temp = temp.time;
    corrected.time.temp[valid.day.temp & !valid.day.temp.time] = med.time +  + times("01:00:00") #when we don't have information about the temperature time, we consider it as deviating by one one
    time.diff.with.med = abs(corrected.time.temp - med.time)
    time.diff.with.med = hours(time.diff.with.med) + minutes(time.diff.with.med)/60
    weights = 1 - 0.5/(1+exp(-(2*l/1.8)*(time.diff.with.med - 1) )) # logistic curve that decreases as time difference with median increases (mid-point = 1h)
  }else{weights = rep(1,n.days)}
  
  #begining of cycle
  w.factor = rep(1, n.days);
  w.factor[time.points <= 5] = 0.5
  w.factor[time.points == 6] = 0.75
  weights = weights * w.factor # to give less weight to temperature at the begining of the cycle
  
  #outliers
  abs.diff = abs(diff(temp.data[which(valid.day.temp)]))
  pmin.abs.diff = pmin(c(abs.diff[1],abs.diff),c(abs.diff, abs.diff[length(abs.diff)]))
  dt = max(1, pmin.abs.diff)
  wo = rep(1,n.days)
  wo[which(valid.day.temp)] = 1-pmin.abs.diff/dt*0.5
  weights = weights * wo
  
  #questionable temperature
  j = which(cycletable$questionable_temp)
  weights[j] = 0.1
  
  if(debug){cat('\t\t weights: ',weights,'\n')}
  
  return(weights)
  
}
