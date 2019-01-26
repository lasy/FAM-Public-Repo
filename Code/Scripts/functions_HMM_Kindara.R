
most_likely_day_of_ovulation_hmm = function(cycletable = cycletable,debug = FALSE, no.print = FALSE, sep = sep.hmm){
  
  ### HMM with temperature measurements as emission
  
  if(debug){cat('start of most_likely_day_of_ovulation_hmm\n')}
  
  ## generating emission probabilities for temperature measurements
  emissionProbs.temp = generate_emissionProbs_from_temp_meas(cycletable = cycletable, debug = debug)
  temp.hmm.symbols = emissionProbs.temp$symbols
  temp.obs = emissionProbs.temp$obs
  T.low = emissionProbs.temp$T.low
  DT = emissionProbs.temp$DT
  weights = emissionProbs.temp$weights
  
  emissionProbs.temp = emissionProbs.temp$matrix
  
  if(debug){cat('\t temp emission prob generated\n')}
  
  emissionProbs.all = combine_emissionProbs_matrices(states = cycle.states,
                                                     symbols.1 = BMC.hmm.symbols,
                                                     symbols.2 = temp.hmm.symbols,
                                                     emissionProbs.1 = emissionProbs.BMC,
                                                     emissionProbs.2 = emissionProbs.temp, sep = sep)
  
  if(debug){cat('\t all emission prob generated\n')}
  
  symbols.all = emissionProbs.all$symbols
  emissionProb.all = emissionProbs.all$matrix
  startProbs = c(1,0,0,0,0,0,0,0,0,0)
  
  hmm.model.all =  initHMM(States = cycle.states,
                           Symbols = symbols.all,
                           startProbs = startProbs,
                           transProbs = cycle.states.transProbs,
                           emissionProbs = emissionProb.all)
  
  if(debug){cat('\t hmm initiated\n')}
  
  
  mucus.observations = mucus.dict$hmm.symbols[match(cycletable$mucus,mucus.dict$index)]
  bleeding.observations = bleeding.dict$hmm.symbols[match(cycletable$bleeding,bleeding.dict$index)]
  #height.observations = height.dict$hmm.symbols[match(cycletable$cervix_height,height.dict$index)]
  #openness.observations = openness.dict$hmm.symbols[match(cycletable$cervix_openness,openness.dict$index)]
  #firmness.observations = firmness.dict$hmm.symbols[match(cycletable$cervix_firmness,firmness.dict$index)]
  #feel.observations = feel.dict$hmm.symbols[match(cycletable$vaginal_sensation,feel.dict$index)]
  
  h = cycletable$cervix_height; h[is.na(h)] = 0;
  o = cycletable$cervix_openness; o[is.na(o)] = 0;
  f = cycletable$cervix_firmness; f[is.na(f)] = 0;
  cervix.observation = pmax(h,o , f)
  cervix.observation = cervix.dict$hmm.symbols[match(cervix.observation,cervix.dict$index)]
  
  obs = paste( bleeding.observations, mucus.observations, cervix.observation, temp.obs, sep = sep) # height.observations,openness.observations,firmness.observations, feel.observations,
  
  N_fields = str_count(obs[1],sep)+1
  
  # EXPAND OBSERVATION TO FILL ALL CYCLEDAYS
  
  all.days = 1:unique(cycletable$length)
  m = match(all.days, cycletable$cycleday)
  obs = obs[m]
  obs[is.na(m)] = paste0(rep("NA",N_fields),collapse = sep)
  weights = weights[m]
  
  # END with a "end" sequence
  
  obs = c(obs, paste0(rep('end',N_fields),collapse = sep))
  
  if(debug){cat('\t before viterbi\n')}
  
  obs.viterbi = viterbi(hmm.model.all,obs)
  if(!no.print){cat(obs.viterbi,'\n')}
  
  if(debug){cat('\t after viterbi\n')}
  
  
  obs.posterior = posterior(hmm.model.all,obs)
  #matplot(t(obs.posterior), type = 'l', col = cycle.states.colors, lty = 1)
  
  #obs.logforward = forward(hmm.model.all,obs)
  #obs.forward = exp(obs.logforward)
  #obs.forward.end = sum(obs.forward[,ncol(obs.forward)])
  #prob.seq = exp(log(obs.forward.end)/(ncol(obs.forward)-1))
  obs.logforward = 0*obs.posterior
  prob.seq = 0
  
  # m.vit = match(obs.viterbi,cycle.states)
  # m.seq = match(obs , symbols.all )
  # P.vit = startProbs[m.vit[1]]*emissionProb.all[m.vit[1],m.seq[1]]
  # for(p in 2:length(obs.viterbi)){
  #   P.vit[p] = P.vit[p-1]*
  #     cycle.states.transProbs[m.vit[p-1],m.vit[p]]*
  #     emissionProb.all[m.vit[p],m.seq[p]]
  # }
  # P.vit = P.vit[length(P.vit)]
  # prob.seq.viterbi = exp(log(P.vit)/(length(obs.viterbi)-1))
  prob.seq.viterbi = 0
  
  x = all.days
  post.ovu = obs.posterior[5,-ncol(obs.posterior)]
  ovu = weighted.mean(x, w = post.ovu)
  ovu.sd = sqrt(sum(post.ovu * (x - ovu)^2))
  
  j = which(obs.viterbi == 'O')
  if(length(j)>0){ovu.viterbi = all.days[j]}else{ovu.viterbi = NA}
  
  r.ovu = round(ovu)
  j = (all.days >= (r.ovu - 3)) & (all.days <= (r.ovu + 3)) 
  ww = weights; ww[is.na(ww)] = 0
  w.weights = sum(ww[j])/sum(j)
  w = dnorm(-3:3,0,2); w = w/sum(w)
  w.missing.temp = as.numeric(!is.na(cycletable$temperature[m][j]))*w
  w.missing.mucus = as.numeric((cycletable$mucus[m][j])!= 0)*w
  w.missing.mucus[is.na(w.missing.mucus)] = 0

  confidence = (w.weights + sum(w.missing.temp) + sum(w.missing.mucus))/3
  
  
  hmm.results.all = list(obs.viterbi = obs.viterbi, 
                         obs.posterior = obs.posterior, 
                         obs.logforward = obs.logforward, 
                         prob.seq = prob.seq, 
                         prob.seq.viterbi = prob.seq.viterbi , 
                         T.low = T.low, 
                         DT = DT,
                         ovu = ovu, ovu.sd = ovu.sd,
                         ovu.viterbi = ovu.viterbi,
                         weights = weights,
                         confidence = confidence)
  
  
  if(debug){cat('end of most_likely_day_of_ovulation_hmm\n')}
  return(results = hmm.results.all)
}



generate_emissionProbs_from_temp_meas = function(cycletable = cycletable, debug = FALSE){
  
  
  weights = compute_weights(cycletable = cycletable)
  
  n.timepoints = sum(!is.na(cycletable$temperature[(weights>0.5)]))
  
  # finding the low & high plateaux
  T.low = quantile(cycletable$temperature[weights>0.5], 0.25, na.rm = TRUE)
  T.high = quantile(cycletable$temperature[weights>0.5], 0.80, na.rm = TRUE)
  DT = T.high - T.low ### question: should DT reflect the noise in the data? how?
  if(!is.na(DT) & (DT < 0.1)){DT = 0}
  
  if((n.timepoints > 5) & !is.na(DT) & (DT>0)){
    
    # defining a temp range going from T.low - DT to T.high + DT
    T.range = seq(floor((T.low-DT)*10)/10, ceiling((T.high+DT)*10)/10, by = 0.1)
    nT = length(T.range)
    
    T.min = T.range[1]; T.max = T.range[nT]
    temp.hmm.symbols = c('NA','LOW',paste(1:nT),'HIGH', 'QT','end')
    
    # converting the temperature to values in 1:nT
    
    m = match(as.character(round(cycletable$temperature,digits = 1)), as.character(T.range))
    Temp = m
    Temp[cycletable$temperature < T.min] = 'LOW'
    Temp[cycletable$temperature > T.max] = 'HIGH'
    Temp[weights < 0.5] = 'QT' # questionable temperature
    
    
    #######################     1 ...... nT
    emissionProbs.temp.hM   = dnorm(T.range, mean = T.low + DT/3, sd = DT/2 )
    emissionProbs.temp.lM   = dnorm(T.range, mean = T.low + DT/4, sd = DT/2 ) 
    emissionProbs.temp.lE   = dnorm(T.range, mean = T.low       , sd = DT/2 ) 
    emissionProbs.temp.hE   = dnorm(T.range, mean = T.low       , sd = DT/2 ) 
    emissionProbs.temp.O    = dnorm(T.range, mean = T.low + DT/4, sd = DT/2 )
    emissionProbs.temp.Rise = dnorm(T.range, mean = T.high- DT/3, sd = DT/2 ) 
    emissionProbs.temp.hP   = dnorm(T.range, mean = T.high      , sd = DT/2 ) 
    emissionProbs.temp.Ep   = dnorm(T.range, mean = T.high- DT/3, sd = DT/2 )
    emissionProbs.temp.lP   = dnorm(T.range, mean = T.low + DT/3, sd = DT/2 ) 
    emissionProbs.temp.end  = rep(0, nT) 
    
    emissionProbs.temp = c()
    for(s in cycle.states){
      eval(parse(text = paste0('emissionProbs.temp = c(emissionProbs.temp,emissionProbs.temp.',s,')')))
    }
    
    emissionProbs.temp = matrix(emissionProbs.temp,
                                nrow = length(cycle.states), ncol = length(emissionProbs.temp.hM), byrow = TRUE)
    
    emissionProbs.temp = emissionProbs.temp/apply(emissionProbs.temp,1,sum)
    emissionProbs.temp.na  = c(5,4,2,1,1,1,2,3,4,0)/10
    emissionProbs.temp.LOW  = c(5,4,3,2,1,1,1,1,3,0)/30
    emissionProbs.temp.HIGH  = c(4,3,2,1,2,2,3,3,2,0)/30
    emissionProbs.temp.QT  = rep(0.1, length(cycle.states))
    emissionProbs.temp.end = c(0,0,0,0,0,0,0,0,0,1)
    emissionProbs.temp = cbind(emissionProbs.temp.na,emissionProbs.temp.LOW,emissionProbs.temp,emissionProbs.temp.HIGH,emissionProbs.temp.QT,emissionProbs.temp.end)
    emissionProbs.temp = emissionProbs.temp/apply(emissionProbs.temp,1,sum)
    emissionProbs.temp[10,] = c(rep(0,length(temp.hmm.symbols)-1),1)
    
    colnames(emissionProbs.temp) = temp.hmm.symbols
    rownames(emissionProbs.temp) = cycle.states
    
  }else{
    T.low = 0;DT = 0; 
    emissionProbs.temp = cbind(c(rep(1,length(cycle.states)-1),0),c(rep(0,length(cycle.states)-1),1))
    Temp = rep('NA',nrow(cycletable))
    temp.hmm.symbols = c('NA','end')
  }
  
  return(list(matrix = emissionProbs.temp, 
              symbols = temp.hmm.symbols, 
              obs = Temp, 
              T.low = T.low, 
              DT = DT,
              weights = weights))
}







