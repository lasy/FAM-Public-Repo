

source('Scripts/functions_HMM.R')
source('Scripts/functions_for_plots.R')
source('Scripts/functions_for_data_cleaning.R')


# UTILITIES

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}

#' Human versions of large currency numbers - extensible via smbl

human_gbp   <- function(x){human_numbers(x, smbl = "£")}
human_usd   <- function(x){human_numbers(x, smbl = "$")}
human_euro  <- function(x){human_numbers(x, smbl = "€")} 
human_num   <- function(x){human_numbers(x, smbl = "")} 



#### local maxima

which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>0,TRUE))>0)
    }else {
      which(diff(diff(x)>0)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)+1
    }
  }
}






# parametric temperature curve



get_latest_filename = function(cycle_id = cycle_id,folder_path = 'Rdata/'){
  filenames = list.files(folder_path,pattern = paste0('^',cycle_id,'_'))
  file = filenames[which(filenames == max(filenames))]
  #cat(file,'\n')
  filename = paste0(folder_path,file)
  return(filename)
}



temp_curve = function(
  param = list(low.t = 36.25, delta.t = 0.4, ovu.day = 14, prog.drop.day = 10, n.day.rise = 2, slope.drop = 1),
  time.vect = seq(1,28,by=0.1), debug = FALSE){
  
  if(debug){cat("temp_curve 1 \n")}
  
  l = log(0.95/0.05)
  
  #cat("param ",param,"\n")
  if(debug){cat("typeof(param)", typeof(param), "\n")}
  if(debug){cat("typeof(param$n.day.rise) ",typeof(param$n.day.rise),"\n")}
  
  slope.rise = 2*l/param$n.day.rise
  
  if(debug){cat("temp_curve 2 \n")}
  
  temp = rep(param$low.t, length(time.vect)) 
  temp = temp + 
    param$delta.t/(1 + exp( - slope.rise * (time.vect-(param$ovu.day + param$n.day.rise/2))))-
    param$delta.t/(1 + exp( -param$slope.drop * (time.vect-(param$ovu.day + param$n.day.rise + param$prog.drop.day + l/param$slope.drop))))
  
  if(debug){cat("temp_curve 3 \n")}
  
  
  #plot(time.vect, temp, type = 'l')
  #i = match(c(min(time.vect):max(time.vect)), time.vect)
  #points(time.vect[i],temp[i],type = 'p', pch = 16)
  
  df = data.frame(time = time.vect, temp = temp, stringsAsFactors = FALSE)
  
  return(df)
  
}






# error on temperature // residues
temp_error = function(temp.data, temp.simu, weights, N.start = 1, N.stop = NA, debug = FALSE){
  if(length(temp.data) != length(temp.simu)){
    stop("length of parameters differ")
  }
  
  
  if(debug){cat('length(temp.data) ',length(temp.data) ,'\n') }
  if(debug){cat('sum(!is.na(temp.data)) ',sum(!is.na(temp.data)) ,'\n') }
  
  if(is.na(N.stop) | (N.stop > length(temp.data))){N.stop = length(temp.data)}
  if(N.start<1){N.start = 1}
  if(N.stop<N.start){return(Inf)}
  
  i = N.start:N.stop
  N = length(i)
  K = sum(!is.na(temp.data[i]))
  
  if(debug){cat('N.start: ',N.start,'\n')}
  if(debug){cat('N.stop: ',N.stop,'\n')}
  if(debug){cat('K: ', K ,'\n') }
  if(debug){cat('N: ', N,'\n') }
  
  
  #err = sum(weights * (temp.data-temp.simu)^2, na.rm = TRUE)
  #err = 1/sum(!is.na(temp.data)) * sum(weights * (temp.data-temp.simu)^2, na.rm = TRUE)
  #err = 1/sum(!is.na(temp.data[i])) * log(sum(weights[i] * (temp.data[i]-temp.simu[i])^2, na.rm = TRUE))
  #err = (N.stop - N.start)/sum(!is.na(temp.data[i])) * log(sum(weights[i] * (temp.data[i]-temp.simu[i])^2, na.rm = TRUE))
  #err = (N.stop - N.start)/sum(!is.na(temp.data[i])) * (sum(weights[i] * (temp.data[i]-temp.simu[i])^2, na.rm = TRUE))
  
  err = (1/sqrt(K)) * sqrt(sum( weights[i] * (temp.data[i]-temp.simu[i])^2 , na.rm = TRUE)) + (1-(K/N))^2
  
  return(err)
}


optim_temp = function(param = param, temp.df, debug = FALSE){
  
  time.points = temp.df$time.points
  temp.data = temp.df$temp
  
  if(debug){cat('here \n')}
  
  param = list(low.t = param[1], delta.t = param[2], 
               ovu.day = param[3], prog.drop.day = param[4], 
               n.day.rise = param[5], slope.drop = param[6])
  
  temp.curve = temp_curve(param = param, time.vect = time.points, debug = debug)
  temp.simu = temp.curve$temp
  
  if(debug){cat('there \n')}
  
  weights = rep(1, length(temp.data))
  
  err = temp_error(temp.data, temp.simu, weights, debug = debug)
  return(err)
}

compute_weights = function(cycletable = cycletable,  debug = FALSE){
  
  time.points = cycletable$out_cycleday
  temp.data = cycletable$temp_c
  n.days = length(time.points)
  
  ## weighting the residues 
  # by reliability of temperature (i.e. giving less weight to temperature that deviates from the median time)
  # by giving less weight to outliers = temperature diverging by more than delta t from their neighbours
  
  valid.day.temp = (temp.data > 35.8)
  weights = rep(1, n.days)
  if(sum(valid.day.temp, na.rm = TRUE)<3){return(weights)}
  
  valid.day.temp.time = valid.day.temp & (cycletable$time_temp != 0)
  med.time = median(cycletable$time_temp[valid.day.temp.time], na.rm = TRUE)
  
  if(!is.na(med.time)){
    #time
    l = log(0.95/0.05)
    corrected.time.temp = cycletable$time_temp;
    corrected.time.temp[(cycletable$temp_c > 35.8) & ((cycletable$time_temp == 0)| is.na(cycletable$time_temp))] = med.time + 1 #when we don't have information about the temperature time, we consider it as deviating by one one
    time.diff.with.med = abs(corrected.time.temp - med.time)
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
  dt = max(0.5, pmin.abs.diff)
  wo = rep(1,n.days)
  wo[which(valid.day.temp)] = 1-pmin.abs.diff/dt*0.5
  weights = weights * wo
  
  if(debug){cat('\t\t weights: ',weights,'\n')}
  
  return(weights)
  
}

optim_ovu = function(param = param, cycletable = cycletable,N.start = 1, N.stop = NA, debug = FALSE){
  
  time.points = cycletable$out_cycleday
  temp.data = cycletable$temp_c
  n.days = length(time.points)
  
  if(debug){cat('here \n')}
  
  param = list(low.t = param[1], delta.t = param[2], 
               ovu.day = param[3], prog.drop.day = param[4], 
               n.day.rise = param[5], slope.drop = param[6])
  
  temp.curve = temp_curve(param = param, time.vect = time.points, debug = debug)
  temp.simu = temp.curve$temp
  
  if(debug){cat('there \n')}
  
  weights = compute_weights(cycletable = cycletable, debug = debug)
  
  err = temp_error(temp.data, temp.simu, weights,N.start = N.start, N.stop = N.stop, debug = debug)
  return(err)
}


set_boundaries_for_param = function(cycletable = cycletable,  debug = FALSE){
  lower = list(low.t = 35, delta.t = 0.15, 
               ovu.day = 5, prog.drop.day = 1, 
               n.day.rise = 1, slope.drop = 0.5)
  upper = list(low.t = 37.5, delta.t = 1, 
               ovu.day = max(cycletable$out_cycleday), prog.drop.day = 200, 
               n.day.rise = 5, slope.drop = 10)
  
  boundaries = list(lower = lower, upper = upper)
  return(boundaries)
}


ovu_guess_from_weighted_temp_average_difference = function(temp = temp, weights = weights){
  
  N.days = length(temp)
  
  if(sum(!is.na(temp))>2){
    mean.foll = c()
    mean.lut = c()
    for(i in 2:N.days){
      mean.foll = c(mean.foll, weighted.mean(x = temp[1:(i-1)], w = weights[1:(i-1)], na.rm = TRUE))
      mean.lut = c(mean.lut, weighted.mean(x = temp[i:N.days], w = weights[i:N.days], na.rm = TRUE))
    }
    
    mean.diff = mean.lut - mean.foll
    o = which.max(mean.diff)
    #defining safe zone
    #zone = cumsum(!is.na(mean.diff))*!is.na(mean.diff)
    zone = cumsum(!is.na(temp))*!is.na(temp)
    
    safe.zone = (zone>3) & (zone<(max(zone)-2)) & c(rep(FALSE,4),rep(TRUE,length(zone)-8) ,rep(FALSE,4))
    if(sum(safe.zone) == 0){safe.zone = (zone == round(max(zone)/2))}
    if(!safe.zone[o]){
      k = which.peaks(mean.diff[safe.zone], decreasing = FALSE, partial = FALSE)
      k = which(safe.zone)[k]
      n.ok = sum(safe.zone[k])
      k = k[safe.zone[k]]
      # there is 1 local max in the safe zone
      if(n.ok == 1){o = k}
      # if several local max in the safe zone > take the biggest
      if(n.ok > 1){km = which.max(mean.diff[k]); o = k[km]}
      # if no local max in the safe zone > take the biggest mean.diff in the safe zone
      if(n.ok < 1){m = which.max(mean.diff[safe.zone]); o = which(safe.zone)[m]}
    }
    delta.t = mean.diff[o]
    low.t = mean.foll[o]
  }else{o = N.days - 13; delta.t = 0.4; low.t = 36.5 }
  return(list(o = o, delta.t = delta.t, low.t = low.t))
}  



most_likely_day_of_ovulation_temp_curve_fitting = function(cycletable = cycletable, n.init.cond = 10, init.param = list(),  Rdata.folder = 'Rdata/', debug = FALSE, no.print = FALSE){
  
  folder.optim.results = paste0(Rdata.folder, 'optim_results/')
  if(!dir.exists(folder.optim.results)){dir.create(folder.optim.results)}
  
  if(debug){cat('most_likely_day_of_ovulation 0 \n')}
  
  # check we only have one cycle in the table
  cycle_ids = unique(cycletable$cycle_id)
  if(length(cycle_ids)!=1){stop("cycletable should have data for 1 cycle only")}
  
  if(debug){cat('most_likely_day_of_ovulation 1 \n')}
  
  
  # replace the unmeasured temperatures by NAs
  j = which(cycletable$temp_c <= 35.8)
  cycletable$temp_c[j] = NA
  cycletable$time_temp[j] = NA
  
  if(debug){cat('most_likely_day_of_ovulation 2 \n')}
  
  
  # set the lower and upper boundariess for the parameters
  boundaries = set_boundaries_for_param(cycletable = cycletable, debug = debug)
  upper = boundaries$upper
  lower = boundaries$lower
  
  if(debug){cat('most_likely_day_of_ovulation 3 \n')}
  
  smallest.error = Inf
  best.optim = c()
  # run the optimization
  for(init.n in 1:n.init.cond){
    
    if(!no.print){cat('initial condition ',init.n, '/',n.init.cond,'\n')}
    if(debug){cat('most_likely_day_of_ovulation init cond. ',init.n,' start \n')}
    
    guess.low.t = min(cycletable$temp_c, na.rm = TRUE)
    guess.delta.t = max(cycletable$temp_c, na.rm = TRUE)-min(cycletable$temp_c, na.rm = TRUE)
    ovu.max = max(cycletable$out_cycleday)
    ovu.min = 3
    guess.ovu.day = init.n/n.init.cond * (ovu.max-ovu.min) + ovu.min
    
    if((n.init.cond == 1)&(length(init.param)>0)){init.param = init.param
    }else{
      init.param = list(low.t = guess.low.t + rnorm(1, mean = 0.2, sd = 0.1) , 
                        delta.t = guess.delta.t + rnorm(1, mean = 0.4-guess.delta.t, sd = 0.2), 
                        ovu.day = guess.ovu.day, 
                        prog.drop.day = 4 + (init.n %% 2) * rnorm(1, mean = 2, sd = 1), 
                        n.day.rise = 2 + (init.n %% 3) * rnorm(1, mean = 2, sd = 1), 
                        slope.drop = 1 + rnorm(1, mean = 0, sd = 0.5) )
    }
    
    if(debug){cat('\t init.param: ',unlist(init.param),' \n')}
    
    opt = optim(par = init.param, fn = optim_ovu, cycletable = cycletable, debug = FALSE,
                method = "L-BFGS-B", lower = lower, upper = upper, hessian = TRUE)
    
    if(debug){cat('\t opt.param: ',opt$par,' \n')}
    
    
    if(debug){cat('most_likely_day_of_ovulation init cond. ',init.n,' optim done \n')}
    
    if(debug){cat('\t error: ',opt$value,'\n')}
    if(debug){cat('\t optim_ovu: ',optim_ovu(param = opt$par, cycletable = cycletable),'\n')}
    
    
    if(opt$value <= smallest.error){
      smallest.error = opt$value
      best.optim = init.n
      #save results of this initial condition
      save(opt, file = paste0(folder.optim.results,'opt_',cycle_ids,'_',init.n,'.Rdata'))
      if(debug){cat('\t best.optim: ',best.optim,'\n')}
      
    }
    
  }
  # take the best results
  rm(opt)
  load(paste0(folder.optim.results,'opt_',cycle_ids,'_',best.optim,'.Rdata'))
  
  # adding SEM to the fit results
  if(debug){cat('getting the sem \n')}
  if(can_be_solved(opt$hessian)){
    if(debug){cat('\t hessian is solvable \n')}
    vc <- solve(opt$hessian) # var-cov matrix 
    se <- sqrt(diag(vc))        # standard errors 
    sem = se/opt$par
  }else{
    if(debug){cat('\t hessian is NOT solvable \n')}
    sem = opt$par/0}
  opt$sem = sem
  
  # adding residues for each HMM generated state
  if(debug){cat('computing the HMM residues \n')}
  hmm.residues = rep(0, length(temp.hmm.symbols))
  names(hmm.residues) = temp.hmm.symbols
  
  ovu.day = round(opt$par[which(names(opt$par) == 'ovu.day')]) 
  n.day.rise = round(opt$par[which(names(opt$par) == 'n.day.rise')]) 
  prog.drop.day = round(opt$par[which(names(opt$par) == 'prog.drop.day')]) 
  if(debug){cat('\t ovu.day:',ovu.day,'\n')}
  if(debug){cat('\t n.day.rise:',n.day.rise,'\n')}
  if(debug){cat('\t prog.drop.day:',prog.drop.day,'\n')}
  
  
  hmm.residues[which(temp.hmm.symbols == 'Foll')] = optim_ovu(param = opt$par, cycletable = cycletable, N.start = max(ovu.day-6,1), N.stop =  ovu.day-1, debug = debug)
  if(debug){cat('\t Foll HMM residues .... done\n')}
  hmm.residues[grep('Ovu', temp.hmm.symbols)] = optim_ovu(param = opt$par, cycletable = cycletable, N.start = ovu.day-3, N.stop =  ovu.day+3, debug = debug)
  hmm.residues[which(temp.hmm.symbols == 'Rise')] = optim_ovu(param = opt$par, cycletable = cycletable, N.start = ovu.day+1, N.stop =  ovu.day+n.day.rise, debug = debug)
  hmm.residues[which(temp.hmm.symbols == 'High')] = optim_ovu(param = opt$par, cycletable = cycletable, N.start = ovu.day+n.day.rise, N.stop =  ovu.day+n.day.rise+prog.drop.day, debug = debug)
  hmm.residues[which(temp.hmm.symbols == 'Drop')] = optim_ovu(param = opt$par, cycletable = cycletable, N.start = min(max(cycletable$out_cycleday),ovu.day+n.day.rise+prog.drop.day+1), N.stop =  max(cycletable$out_cycleday), debug = debug)
  
  opt$hmm.residues = hmm.residues
  
  if(debug){cat('computing the HMM residues .... done\n')}
  
  return(opt)
}







