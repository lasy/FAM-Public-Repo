

most_likely_day_of_ovulation_hmm = function(cycletable = cycletable,
                                            debug = FALSE, 
                                            no.print = FALSE, 
                                            sep = sep.hmm,
                                            random_transition_probabilities = FALSE,
                                            noise = 0,
                                            conservative_transition_probabilities = FALSE,
                                            C = 0){
  
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
                                                     symbols.1 = MBC.hmm.symbols,
                                                     symbols.2 = temp.hmm.symbols,
                                                     emissionProbs.1 = emissionProbs.MBC,
                                                     emissionProbs.2 = emissionProbs.temp, sep = sep)
  
  if(debug){cat('\t all emission prob generated\n')}
  
  symbols.all = emissionProbs.all$symbols
  emissionProb.all = emissionProbs.all$matrix
  startProbs = c(1,0,0,0,0,0,0,0,0,0)
  
  # if we want to add noise on the transition probabilities
  if(random_transition_probabilities){
    
    noise.mat = 10^(rnorm(nrow(cycle.states.transProbs) * ncol(cycle.states.transProbs),
                      mean = 0, sd = noise))
    noise.mat = matrix(noise.mat, ncol = ncol(cycle.states.transProbs), nrow = nrow(cycle.states.transProbs))
    cycle.states.transProbs.n = cycle.states.transProbs * noise.mat
    cycle.states.transProbs.n = cycle.states.transProbs.n/apply(cycle.states.transProbs.n,1,sum)
  }else{
    cycle.states.transProbs.n = cycle.states.transProbs
  }
  
  
  # if we want more conservative transition probabilities
  if(conservative_transition_probabilities){
    conservative.mat = diag(rep(C, nrow(cycle.states.transProbs)))
    cycle.states.transProbs.n = cycle.states.transProbs + diag(cycle.states.transProbs) * conservative.mat
    cycle.states.transProbs.n = cycle.states.transProbs.n/apply(cycle.states.transProbs.n,1,sum)
  }else{
    cycle.states.transProbs.n = cycle.states.transProbs
  }
  
  
  cycle.states.transProbs.simpl = cycle.states.transProbs.n
  cycle.states.transProbs.simpl[cycle.states.transProbs.simpl==0] = NA
  dev.sym = sum(apply(cycle.states.transProbs.simpl,1,sd, na.rm = TRUE), na.rm = TRUE)
  
  # building the HMM model
  
  hmm.model.all =  initHMM(States = cycle.states,
                           Symbols = symbols.all,
                           startProbs = startProbs,
                           transProbs = cycle.states.transProbs.n,
                           emissionProbs = emissionProb.all)
  
  if(debug){cat('\t hmm initiated\n')}
  

  mucus.observations = mucus.dict$hmm.symbols[match(cycletable$elixir,mucus.dict$index)]
  bleeding.observations = bleeding.dict$hmm.symbols[match(cycletable$blood,bleeding.dict$index)]
  cervix.observations = cervix.dict$hmm.symbols[match(cycletable$feel,cervix.dict$index)]
  
  obs = paste(mucus.observations, bleeding.observations, cervix.observations, temp.obs, sep = sep)
  obs = c(obs, paste0(rep('end',4),collapse = sep))

  if(debug){cat('\t before viterbi\n')}
  
  obs.viterbi = viterbi(hmm.model.all,obs)
  if(!no.print){cat(obs.viterbi,'\n')}
  if(length(grep('end',obs.viterbi))==0){do.temp.fit = FALSE}
  
  if(debug){cat('\t after viterbi\n')}
  
  
  obs.posterior = posterior(hmm.model.all,obs)
  #matplot(t(obs.posterior), type = 'l', col = cycle.states.colors, lty = 1)
  
  obs.logforward = forward(hmm.model.all,obs)
  obs.forward = exp(obs.logforward)
  obs.forward.end = sum(obs.forward[,ncol(obs.forward)])
  prob.seq = exp(log(obs.forward.end)/(ncol(obs.forward)-1))
  
  m.vit = match(obs.viterbi,cycle.states)
  m.seq = match(obs , symbols.all )
  P.vit = startProbs[m.vit[1]]*emissionProb.all[m.vit[1],m.seq[1]]
  for(p in 2:length(obs.viterbi)){
    P.vit[p] = P.vit[p-1]*
      cycle.states.transProbs[m.vit[p-1],m.vit[p]]*
      emissionProb.all[m.vit[p],m.seq[p]]
  }
  P.vit = P.vit[length(P.vit)]
  prob.seq.viterbi = exp(log(P.vit)/(length(obs.viterbi)-1))
  
  x = cycletable$out_cycleday
  post.ovu = obs.posterior[5,-ncol(obs.posterior)]
  ovu = weighted.mean(x, w = post.ovu)
  ovu.sd = sqrt(sum(post.ovu * (x - ovu)^2))
  
  j = which(obs.viterbi == 'O')
  if(length(j)>0){ovu.viterbi = cycletable$out_cycleday[j]}else{ovu.viterbi = NA}
  
  r.ovu = round(ovu)
  j = (cycletable$out_cycleday >= (r.ovu - 3)) & (cycletable$out_cycleday <= (r.ovu + 3)) 
  ww = weights; ww[is.na(ww)] = 0
  w.weights = sum(ww[j])/sum(j)
  w = dnorm(-3:3,0,2); w = w/sum(w)
  w.missing.temp = as.numeric(!is.na(cycletable$temp_c[j]))*w
  w.missing.elixir = as.numeric((cycletable$elixir[j])!= 0)*w
#  w.feel = as.numeric((cycletable$feel[j])!= 0)*w/2
#  w.blood = as.numeric((cycletable$blood[j])!= 0)*w/4
  confidence = (w.weights + sum(w.missing.temp) + sum(w.missing.elixir))/3
  
  
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
                         confidence = confidence,
                         dev.sym = dev.sym)
  

  if(debug){cat('end of most_likely_day_of_ovulation_hmm\n')}
  return(results = hmm.results.all)
}




generate_emissionProbs_from_temp_meas = function(cycletable = cycletable, debug = FALSE){
  
  
  weights = compute_weights(cycletable = cycletable)
  
  n.timepoints = sum(!is.na(cycletable$temp_c[(weights>0.5)]))
  
  # finding the low & high plateaux
  T.low = quantile(cycletable$temp_c[weights>0.5], 0.25, na.rm = TRUE)
  T.high = quantile(cycletable$temp_c[weights>0.5], 0.80, na.rm = TRUE)
  DT = T.high - T.low ### question: should DT reflect the noise in the data? how?
  
  
  if((n.timepoints > 5) & !is.na(DT) & (DT>0)){
    
    # defining a temp range going from T.low - DT to T.high + DT
    T.range = seq(floor((T.low-DT)*10)/10, ceiling((T.high+DT)*10)/10, by = 0.05)
    nT = length(T.range)
    
    T.min = T.range[1]; T.max = T.range[nT]
    temp.hmm.symbols = c('NA','LOW',paste(1:nT),'HIGH', 'QT','end')
    
    # converting the temperature to values in 1:nT
    
    m = match(as.character(cycletable$temp_c), as.character(T.range))
    Temp = m
    Temp[cycletable$temp_c < T.min] = 'LOW'
    Temp[cycletable$temp_c > T.max] = 'HIGH'
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





# below is not used anymore


most_likely_day_of_ovulation = function(cycletable = cycletable, n.init.cond = 10, init.param = list(), Rdata.folder = 'Rdata/',debug = FALSE, no.print = FALSE){
  
  #if more than 5 temperature measurements
  n.temp = sum(cycletable$temp_c>35.8, na.rm = TRUE)
  if(n.temp >=5){do.temp.fit = TRUE}else{do.temp.fit = FALSE}
  if(do.temp.fit){
    opt = most_likely_day_of_ovulation_temp_curve_fitting(cycletable = cycletable, n.init.cond = n.init.cond, init.param = init.param,  Rdata.folder = Rdata.folder,debug = debug, no.print = no.print)
  }
  ### HMM with Mucus,Bleeding and Cervix 
  if(debug){cat('start of HMM MBC\n')}
  source("HMM_menstrual_cycle.R")
  hmm.model.MBC =  initHMM(States = cycle.states,
                           Symbols = MBC.hmm.symbols,
                           startProbs = c(1,0,0,0,0,0,0,0,0,0),
                           transProbs = cycle.states.transProbs,
                           emissionProbs = emissionProbs.MBC
  )
  hmm.results.MBC = detect_ovu_hmm(cycletable = cycletable, hmm.model = hmm.model.MBC, debug = debug)
  
  if(debug){cat(hmm.results.MBC$obs.viterbi, '\n')}
  
  if(debug){cat('end of HMM MBC\n')}
  
  
  ### HMM adding temperature curve fitting results 
  
  #sem = opt$sem
  #sem = sem[which(names(sem) == 'ovu.day')]
  #m = 1/sem*5
  #if(debug){cat('m',m,'\n')}
  
  if(do.temp.fit){
    if(debug){cat('start of HMM all\n')}
    
    emissionProbs.temp = generate_emissionProbs_from_temp_optim(opt = opt, debug = debug)
    
    if(debug){cat('\t temp emission prob generated\n')}
    
    emissionProbs.all = combine_emissionProbs_matrices(states = cycle.states,
                                                       symbols.1 = MBC.hmm.symbols,
                                                       symbols.2 = temp.hmm.symbols,
                                                       emissionProbs.1 = emissionProbs.MBC,
                                                       emissionProbs.2 = emissionProbs.temp)
    
    if(debug){cat('\t all emission prob generated\n')}
    
    symbols.all = emissionProbs.all$symbols
    emissionProb.all = emissionProbs.all$matrix
    startProbs = c(1,0,0,0,0,0,0,0,0,0)
    
    hmm.model.all =  initHMM(States = cycle.states,
                             Symbols = symbols.all,
                             startProbs = startProbs,
                             transProbs = cycle.states.transProbs,
                             emissionProbs = emissionProb.all
    )
    
    if(debug){cat('\t hmm initiated\n')}
    
    temp.obs = generate_obs_from_temp_optim(param = opt$par, lc = max(cycletable$out_cycleday))
    if(debug){cat('\t temp.obs: ',temp.obs, '\n')}
    opt$temp.obs = temp.obs
    obs =  paste(hmm.results.MBC$obs, temp.obs, sep = '.')
    
    
    obs.viterbi = viterbi(hmm.model.all,obs)
    if(!no.print){cat(obs.viterbi,'\n')}
    if(length(grep('end',obs.viterbi))==0){do.temp.fit = FALSE}
    
    obs.posterior = posterior(hmm.model.all,obs)
    #matplot(t(obs.posterior), type = 'l', col = cycle.states.colors, lty = 1)
    
    obs.logforward = forward(hmm.model.all,obs)
    obs.forward = exp(obs.logforward)
    obs.forward.end = sum(obs.forward[,ncol(obs.forward)])
    prob.seq = exp(log(obs.forward.end)/(ncol(obs.forward)-1))
    
    m.vit = match(obs.viterbi,cycle.states)
    m.seq = match(obs , symbols.all )
    P.vit = startProbs[m.vit[1]]*emissionProb.all[m.vit[1],m.seq[1]]
    for(p in 2:length(obs.viterbi)){
      P.vit[p] = P.vit[p-1]*
        cycle.states.transProbs[m.vit[p-1],m.vit[p]]*
        emissionProb.all[m.vit[p],m.seq[p]]
    }
    P.vit = P.vit[length(P.vit)]
    
    
    #matplot(t(exp(obs.forward)), type = 'l', col = cycle.states.colors, lty= 1)
    #matplot(t(obs.forward), type = 'l', col = cycle.states.colors, lty= 1)
    #apply(exp(obs.forward),2,sum)
    
    hmm.results.all = list(obs.viterbi = obs.viterbi, obs.posterior = obs.posterior, obs.logforward = obs.logforward, prob.seq = prob.seq, prob.seq.viterbi = P.vit, prob.seq.viterbi.norm = exp(log(P.vit)/(length(obs.viterbi)-1)) )
    
    if(debug){cat('end of HMM all\n')}
  }
  
  ##### putting it all together
  
  if(debug){cat('end of most_likely_day_of_ovulation\n')}
  if(do.temp.fit){
    return(results = list(do.temp.fit = do.temp.fit, opt = opt, hmm.results.MBC = hmm.results.MBC, hmm.results.all = hmm.results.all))
  }else{
    return(results = list(do.temp.fit = do.temp.fit, hmm.results.MBC = hmm.results.MBC))
  }
}





detect_ovu_hmm = function(cycletable = cycletable, hmm.model = hmm.model, debug = TRUE){
  
  source("HMM_menstrual_cycle.R")
  
  mucus.observations = mucus.dict$hmm.symbols[match(cycletable$elixir,mucus.dict$index)]
  bleeding.observations = bleeding.dict$hmm.symbols[match(cycletable$blood,bleeding.dict$index)]
  cervix.observations = cervix.dict$hmm.symbols[match(cycletable$feel,cervix.dict$index)]
  
  MBC.observations = paste(mucus.observations, bleeding.observations, cervix.observations, sep = '.')
  MBC.observations = c(MBC.observations, 'end.end.end')
  
  obs.viterbi = viterbi(hmm.model,MBC.observations)
  #cat(obs.viterbi,'\n')
  
  obs.posterior = posterior(hmm.model,MBC.observations)
  #matplot(t(obs.posterior), type = 'l', col = cycle.states.colors, lty = 1)
  
  return(list(obs.viterbi = obs.viterbi, obs.posterior = obs.posterior, obs = MBC.observations))
}



generate_emissionProbs_from_temp_optim = function(opt = opt, debug = FALSE){
  
  hmm.residues = opt$hmm.residues
  hmm.residues[!is.finite(hmm.residues)] = 1000
  hmm.residues = pmax(pmin(hmm.residues, 1000),0.01)
  
  f = 1/hmm.residues[1]
  O = 1/hmm.residues[3]
  R = 1/hmm.residues[5]
  H = 1/hmm.residues[6]
  D = 1/hmm.residues[7]
  if(hmm.residues[7] == 1000){D = 0}
  
  temp.hmm.symbols = c('Foll','BOvu','Ovu', 'AOvu','Rise','High','Drop','end')
  
  #######################     Foll BOvu Ovu    AOvu    Rise High  Drop end 
  emissionProbs.temp.hM   = c(1+f ,1    ,1     ,1    ,1     ,1    ,1    ,0) 
  emissionProbs.temp.lM   = c(1+f ,1+O/5,1     ,1    ,1     ,1    ,1    ,0) 
  emissionProbs.temp.lE   = c(1+f ,1+O/2,1+O/5 ,1    ,1     ,1    ,1    ,0) 
  emissionProbs.temp.hE   = c(1+f ,1+O  ,1+O/2 ,1    ,1     ,1    ,1    ,0) 
  emissionProbs.temp.O    = c(1   ,1+O/2,1+O   ,1+O/2,1+R/5 ,1    ,1    ,0) 
  emissionProbs.temp.Rise = c(1   ,1    ,1+O/2 ,1+O  ,1+R   ,1+H/5,1    ,0) 
  emissionProbs.temp.hP   = c(1   ,1    ,1+O/2 ,1+O  ,1+R   ,1+2*H,1+D/5,0) 
  emissionProbs.temp.Ep   = c(1   ,1    ,1     ,1+O/5,1+R/5 ,1+H/2,1+D/3,0) 
  emissionProbs.temp.lP   = c(1   ,1    ,1     ,1    ,1     ,1    ,1+D  ,0) 
  emissionProbs.temp.end  = c(0   ,0    ,0     ,0    ,0     ,0    ,0    ,1) 
  
  
  emissionProbs.temp = c()
  for(s in cycle.states){
    eval(parse(text = paste0('emissionProbs.temp = c(emissionProbs.temp,emissionProbs.temp.',s,')')))
  }
  
  emissionProbs.temp = matrix(emissionProbs.temp,
                              nrow = length(cycle.states), ncol = length(temp.hmm.symbols)
                              , byrow = TRUE, dimnames = list(cycle.states, temp.hmm.symbols))
  
  
  emissionProbs.temp = emissionProbs.temp/apply(emissionProbs.temp,1,sum)
  return(emissionProbs.temp)
}




can_be_solved = function(m) class(try(solve(m),silent=T))=="matrix"



generate_obs_from_temp_optim = function(param = param, lc = 28, debug = FALSE){
  
  ovu = round(param[which(names(param) == 'ovu.day')])
  obs = rep('Foll', ovu-2)
  obs = c(obs,'BOvu','Ovu','AOvu')
  n.Rise = max(round(param[which(names(param) == 'n.day.rise')])-2,0)
  obs = c(obs, rep('Rise', n.Rise))
  n.High = max(min(lc-length(obs)-1, 
                   round(param[which(names(param) == 'prog.drop.day')])),0)
  obs = c(obs, rep('High', n.High))
  n.drop = max(0,lc -length(obs))
  obs = c(obs, rep('Drop', n.drop), 'end')
  
  return(obs)
  
}


