#### HMM
hmm_par = list()

hmm_par$sep.hmm = '_'


hmm_par$cycle.states = c('hM','lM','lE','hE','O','Rise','hP','Ep','lP','end')
hmm_par$cycle.states.colors = c(viz_cols$dark.red,viz_cols$light.red,viz_cols$light.blue,viz_cols$blue,'black','lightgoldenrod3',viz_cols$yellow,'darkseagreen3',viz_cols$light.yellow,'gray')

hmm_par$cycle.states.dict = data.frame(states =  hmm_par$cycle.states, colors = hmm_par$cycle.states.colors, stringsAsFactors = FALSE)


# TRANSITION PROBABILITIES

#                               hM   lM   lE  hE  O  Rise hP   Ep  lP  end
cycle.states.transProbs.hM  = c(0.5 ,0.5 ,0  ,0  ,0  ,0  ,0   ,0  ,0   ,0)
cycle.states.transProbs.lM  = c(0   ,0.5 ,0.5,0  ,0  ,0  ,0   ,0  ,0   ,0)
cycle.states.transProbs.lE  = c(0   ,0   ,0.5,0.5,0  ,0  ,0   ,0  ,0   ,0)
cycle.states.transProbs.hE  = c(0   ,0   ,1/3,1/3,1/3,0  ,0   ,0  ,0   ,0)
cycle.states.transProbs.O   = c(0   ,0   ,0  ,0  ,0  ,0.5,0.5 ,0  ,0   ,0)
cycle.states.transProbs.Rise= c(0   ,0   ,0  ,0  ,0  ,0.4,0.6 ,0  ,0   ,0)
cycle.states.transProbs.hP  = c(0   ,0   ,0  ,0  ,0  ,0  ,0.55,0.2,0.2 ,0.05)
cycle.states.transProbs.Ep  = c(0   ,0   ,0  ,0  ,0  ,0  ,0.35,0.3,0.35,0)
cycle.states.transProbs.lP  = c(0   ,0   ,0  ,0  ,0  ,0  ,0   ,0  ,0.5 ,0.5)
cycle.states.transProbs.end = c(0   ,0   ,0  ,0  ,0  ,0  ,0   ,0  ,0   ,1)


hmm_par$cycle.states.transProbs = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$cycle.states.transProbs = c(hmm_par$cycle.states.transProbs,cycle.states.transProbs.',s,')')))
}

hmm_par$cycle.states.transProbs = matrix(hmm_par$cycle.states.transProbs,10, byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cycle.states))

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(cycle.states.transProbs.',s,')')))
}


## MUCUS

hmm_par$mucus.hmm.symbols = c('NA','No',paste0('s',1:3),paste0('c',1:3),paste0('e',1:3),paste0('w',1:3),'end')

###########################  NA No   s1  s2  s3  c1  c2  c3  e1  e2  e3  w1  w2  w3  end
emissionProbs.mucus.hM   = c(10 ,8  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,0) 
emissionProbs.mucus.lM   = c(10 ,8  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,1  ,0) 
emissionProbs.mucus.lE   = c(10 ,10 ,4  ,2  ,1  ,8  ,7  ,5  ,2  ,1  ,1  ,2  ,1  ,1  ,0)
emissionProbs.mucus.hE   = c(3  ,3  ,1  ,1  ,1  ,2  ,3  ,4  ,8  ,9  ,10 ,8  ,8  ,8  ,0)  
emissionProbs.mucus.O    = c(3  ,3  ,5  ,6  ,7  ,1  ,2  ,2  ,10 ,10 ,10  ,10 ,10 ,10 ,0)
emissionProbs.mucus.Rise = c(3  ,5  ,7  ,7  ,7  ,2  ,2  ,3  ,7  ,7  ,6  ,6  ,5  ,3  ,0)
emissionProbs.mucus.hP   = c(8  ,8  ,8  ,6  ,3  ,6  ,7  ,5  ,1  ,1  ,1  ,1  ,1  ,1  ,0)
emissionProbs.mucus.Ep   = c(6  ,6  ,8  ,3  ,1  ,5  ,5  ,5  ,2  ,2  ,1  ,2  ,1  ,1  ,0)
emissionProbs.mucus.lP   = c(10 ,10 ,3  ,2  ,1  ,4  ,3  ,2  ,1  ,1  ,1  ,1  ,1  ,1  ,0) 
emissionProbs.mucus.end  = c(0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,1) 


hmm_par$emissionProbs.mucus = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.mucus = c(hmm_par$emissionProbs.mucus,emissionProbs.mucus.',s,')')))
}

hmm_par$emissionProbs.mucus = matrix(hmm_par$emissionProbs.mucus,
                             nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$mucus.hmm.symbols)
                             , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$mucus.hmm.symbols))

hmm_par$emissionProbs.mucus = hmm_par$emissionProbs.mucus/ apply(hmm_par$emissionProbs.mucus, 1, sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.mucus.',s,')')))
}




## BLEEDING

hmm_par$bleeding.hmm.symbols = c('NA','3','2','1','s','end')

###########################     NA   3   2    1     s    end 
emissionProbs.bleeding.hM   = c(0.2 ,0.6 ,0.6 ,0.01,0.01,0) 
emissionProbs.bleeding.lM   = c(0.19,0.01,0.4 ,0.4 ,0.3 ,0) 
emissionProbs.bleeding.lE   = c(0.85,0.05,0.05,0.05,0.1 ,0) 
emissionProbs.bleeding.hE   = c(0.85,0.05,0.05,0.05,0.1 ,0) 
emissionProbs.bleeding.O    = c(0.70,0.05,0.05,0.10,0.15,0)
emissionProbs.bleeding.Rise = c(0.75,0.05,0.05,0.07,0.15,0) 
emissionProbs.bleeding.hP   = c(0.85,0.05,0.05,0.05,0.05,0) 
emissionProbs.bleeding.Ep   = c(0.85,0.05,0.05,0.05,0.05,0) 
emissionProbs.bleeding.lP   = c(0.60,0.05,0.05,0.2 ,0.3 ,0)
emissionProbs.bleeding.end  = c(0   ,0   ,0   ,0   ,0   ,1) 


hmm_par$emissionProbs.bleeding = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.bleeding = c(hmm_par$emissionProbs.bleeding,emissionProbs.bleeding.',s,')')))
}

hmm_par$emissionProbs.bleeding = matrix(hmm_par$emissionProbs.bleeding,
                                nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$bleeding.hmm.symbols)
                                , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$bleeding.hmm.symbols))


hmm_par$emissionProbs.bleeding = hmm_par$emissionProbs.bleeding/ apply(hmm_par$emissionProbs.bleeding, 1, sum)


for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.bleeding.',s,')')))
}


## CERVIX 

# openness

hmm_par$cervix.openness.hmm.symbols = c('NA','C','M','O','end')

###########################   NA   C   M   O  end 
emissionProbs.cervix.openness.hM   = c(10  ,2  ,2  ,3  ,0)  
emissionProbs.cervix.openness.lM   = c(10  ,2  ,2  ,3  ,0)   
emissionProbs.cervix.openness.lE   = c(10  ,7  ,7  ,2  ,0)   
emissionProbs.cervix.openness.hE   = c(10  ,1  ,5  ,10 ,0)  
emissionProbs.cervix.openness.O    = c(10  ,1  ,10 ,8  ,0)  
emissionProbs.cervix.openness.Rise = c(10  ,5  ,10 ,3  ,0)  
emissionProbs.cervix.openness.hP   = c(10  ,10 ,2  ,1  ,0)  
emissionProbs.cervix.openness.Ep   = c(10  ,8  ,4  ,2  ,0)  
emissionProbs.cervix.openness.lP   = c(10  ,10 ,8  ,2  ,0)  
emissionProbs.cervix.openness.end  = c(0   ,0  ,0  ,0  ,1)   


hmm_par$emissionProbs.cervix.openness = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.cervix.openness = c(hmm_par$emissionProbs.cervix.openness,emissionProbs.cervix.openness.',s,')')))
}

hmm_par$emissionProbs.cervix.openness = matrix(hmm_par$emissionProbs.cervix.openness,
                              nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$cervix.openness.hmm.symbols)
                              , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cervix.openness.hmm.symbols))

hmm_par$emissionProbs.cervix.openness = hmm_par$emissionProbs.cervix.openness/apply(hmm_par$emissionProbs.cervix.openness,1,sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.openness.',s,')')))
}



# firmness

hmm_par$cervix.firmness.hmm.symbols = c('NA','F','M','S','end')

###########################            NA   F   M   S  end 
emissionProbs.cervix.firmness.hM   = c(10  ,2  ,2  ,3  ,0)  
emissionProbs.cervix.firmness.lM   = c(10  ,2  ,2  ,3  ,0)   
emissionProbs.cervix.firmness.lE   = c(10  ,7  ,7  ,2  ,0)   
emissionProbs.cervix.firmness.hE   = c(10  ,1  ,5  ,10 ,0)  
emissionProbs.cervix.firmness.O    = c(10  ,1  ,10 ,8  ,0)  
emissionProbs.cervix.firmness.Rise = c(10  ,5  ,10 ,3  ,0)  
emissionProbs.cervix.firmness.hP   = c(10  ,10 ,2  ,1  ,0)  
emissionProbs.cervix.firmness.Ep   = c(10  ,8  ,4  ,2  ,0)  
emissionProbs.cervix.firmness.lP   = c(10  ,10 ,8  ,6  ,0)  
emissionProbs.cervix.firmness.end  = c(0   ,0  ,0  ,0  ,1)   


hmm_par$emissionProbs.cervix.firmness = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.cervix.firmness = c(hmm_par$emissionProbs.cervix.firmness,emissionProbs.cervix.firmness.',s,')')))
}

hmm_par$emissionProbs.cervix.firmness = matrix(hmm_par$emissionProbs.cervix.firmness,
                                       nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$cervix.firmness.hmm.symbols)
                                       , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cervix.firmness.hmm.symbols))

hmm_par$emissionProbs.cervix.firmness = hmm_par$emissionProbs.cervix.firmness/apply(hmm_par$emissionProbs.cervix.firmness,1,sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.firmness.',s,')')))
}



# height

hmm_par$cervix.height.hmm.symbols = c('NA','L','M','H','end')

###########################          NA   L   M   H  end 
emissionProbs.cervix.height.hM   = c(10  ,2  ,2  ,3  ,0)  
emissionProbs.cervix.height.lM   = c(10  ,2  ,2  ,3  ,0)   
emissionProbs.cervix.height.lE   = c(10  ,2  ,3  ,2  ,0)   
emissionProbs.cervix.height.hE   = c(10  ,5  ,8  ,8  ,0)  
emissionProbs.cervix.height.O    = c(10  ,2  ,7  ,10 ,0)  
emissionProbs.cervix.height.Rise = c(10  ,4  ,6  ,6  ,0)  
emissionProbs.cervix.height.hP   = c(10  ,3  ,3  ,3  ,0)  
emissionProbs.cervix.height.Ep   = c(10  ,2  ,3  ,2  ,0)  
emissionProbs.cervix.height.lP   = c(10  ,5  ,5  ,5  ,0)  
emissionProbs.cervix.height.end  = c(0   ,0  ,0  ,0  ,1)   


hmm_par$emissionProbs.cervix.height = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.cervix.height = c(hmm_par$emissionProbs.cervix.height,emissionProbs.cervix.height.',s,')')))
}

hmm_par$emissionProbs.cervix.height = matrix(hmm_par$emissionProbs.cervix.height,
                                       nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$cervix.height.hmm.symbols)
                                       , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cervix.height.hmm.symbols))

hmm_par$emissionProbs.cervix.height = hmm_par$emissionProbs.cervix.height/apply(hmm_par$emissionProbs.cervix.height,1,sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.height.',s,')')))
}



# CERVIX OVERALL - max of opennes, firmness and height


hmm_par$cervix.hmm.symbols = c('NA','I','M','F','end')

###########################   NA   I   M   F  end 
emissionProbs.cervix.hM   = c(10  ,1  ,2  ,1  ,0)  
emissionProbs.cervix.lM   = c(10  ,2  ,3  ,2  ,0)   
emissionProbs.cervix.lE   = c(10  ,2  ,3  ,2  ,0)   
emissionProbs.cervix.hE   = c(10  ,3  ,8  ,9  ,0)  
emissionProbs.cervix.O    = c(10  ,2  ,7  ,10 ,0)  
emissionProbs.cervix.Rise = c(10  ,4  ,8  ,6  ,0)  
emissionProbs.cervix.hP   = c(10  ,6  ,3  ,2  ,0)  
emissionProbs.cervix.Ep   = c(10  ,2  ,3  ,2  ,0)  
emissionProbs.cervix.lP   = c(10  ,3  ,4  ,5  ,0)  
emissionProbs.cervix.end  = c(0   ,0  ,0  ,0  ,1)   


hmm_par$missionProbs.cervix = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.cervix = c(hmm_par$emissionProbs.cervix,emissionProbs.cervix.',s,')')))
}

hmm_par$emissionProbs.cervix = matrix(hmm_par$emissionProbs.cervix,
                                     nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$cervix.hmm.symbols)
                                     , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cervix.hmm.symbols))

hmm_par$emissionProbs.cervix = hmm_par$emissionProbs.cervix/apply(hmm_par$emissionProbs.cervix,1,sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.',s,')')))
}





## VAGINAL SENSATION 

hmm_par$feel.hmm.symbols = c('NA','D','DS','Wm','Wl','end')

##########################  NA   D   Ds  Wm  Wl end 
emissionProbs.feel.hM   = c(10  ,1  ,1  ,1  ,1  ,0)  
emissionProbs.feel.lM   = c(10  ,1  ,1  ,1  ,1  ,0)   
emissionProbs.feel.lE   = c(10  ,3  ,2  ,2  ,1  ,0)  
emissionProbs.feel.hE   = c(10  ,1  ,1  ,4  ,2  ,0)    
emissionProbs.feel.O    = c(10  ,1  ,1  ,4  ,5  ,0) 
emissionProbs.feel.Rise = c(10  ,1  ,2  ,4  ,2  ,0)
emissionProbs.feel.hP   = c(10  ,3  ,4  ,2  ,1  ,0)
emissionProbs.feel.Ep   = c(10  ,2  ,3  ,2  ,1  ,0)  
emissionProbs.feel.lP   = c(10  ,3  ,3  ,1  ,1  ,0)  
emissionProbs.feel.end  = c(0   ,0  ,0  ,0  ,0  ,1)   


hmm_par$emissionProbs.feel = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.feel = c(hmm_par$emissionProbs.feel,emissionProbs.feel.',s,')')))
}

hmm_par$emissionProbs.feel = matrix(hmm_par$emissionProbs.feel,
                                       nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$feel.hmm.symbols)
                                       , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$feel.hmm.symbols))

hmm_par$emissionProbs.feel = hmm_par$emissionProbs.feel/apply(hmm_par$emissionProbs.feel,1,sum)

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.feel.',s,')')))
}






##############################
# COMBINATION BLEEDING + MUCUS


hmm_par$BM = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                    symbols.1 = hmm_par$bleeding.hmm.symbols, 
                                    symbols.2 = hmm_par$mucus.hmm.symbols,
                                    emissionProbs.1 = hmm_par$emissionProbs.bleeding,
                                    emissionProbs.2 = hmm_par$emissionProbs.mucus,
                                    sep = hmm_par$sep.hmm)

##############################
# COMBINATION BLEEDING + CERVIX

hmm_par$BMC = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                      symbols.1 = hmm_par$BM$symbols, 
                                      symbols.2 = hmm_par$cervix.hmm.symbols,
                                      emissionProbs.1 = hmm_par$BM$matrix,
                                      emissionProbs.2 = hmm_par$emissionProbs.cervix,
                                      sep = hmm_par$sep.hmm)


hmm_par$BMC.hmm.symbols = hmm_par$BMC$symbols
hmm_par$emissionProbs.BMC = hmm_par$BMC$matrix





##############################
# COMBINATION BLEEDING + CERVIX + FEEL


hmm_par$BMCF = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                         symbols.1 = hmm_par$BMC$symbols, 
                                         symbols.2 = hmm_par$feel.hmm.symbols,
                                         emissionProbs.1 = hmm_par$BMC$matrix,
                                         emissionProbs.2 = hmm_par$emissionProbs.feel,
                                         sep = hmm_par$sep.hmm)



hmm_par$BMCF.hmm.symbols = hmm_par$BMCF$symbols
hmm_par$emissionProbs.BMCF = hmm_par$BMCF$matrix




##############################
# COMBINATION BLEEDING + CERVIX (all 3 variables)

hmm_par$BMCh = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                      symbols.1 = hmm_par$BM$symbols, 
                                      symbols.2 = hmm_par$cervix.height.hmm.symbols,
                                      emissionProbs.1 = hmm_par$BM$matrix,
                                      emissionProbs.2 = hmm_par$emissionProbs.cervix.height,
                                      sep = hmm_par$sep.hmm)

hmm_par$BMCho = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                      symbols.1 = hmm_par$BMCh$symbols, 
                                      symbols.2 = hmm_par$cervix.openness.hmm.symbols,
                                      emissionProbs.1 = hmm_par$BMCh$matrix,
                                      emissionProbs.2 = hmm_par$emissionProbs.cervix.openness,
                                      sep = hmm_par$sep.hmm)

hmm_par$BMChof = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                       symbols.1 = hmm_par$BMCho$symbols, 
                                       symbols.2 = hmm_par$cervix.firmness.hmm.symbols,
                                       emissionProbs.1 = hmm_par$BMCho$matrix,
                                       emissionProbs.2 = hmm_par$emissionProbs.cervix.firmness,
                                       sep = hmm_par$sep.hmm)


##############################
# COMBINATION BLEEDING + CERVIX + FEEL


hmm_par$BMChofF = combine_emissionProbs_matrices(states = hmm_par$cycle.states, 
                                        symbols.1 = hmm_par$BMChof$symbols, 
                                        symbols.2 = hmm_par$feel.hmm.symbols,
                                        emissionProbs.1 = hmm_par$BMChof$matrix,
                                        emissionProbs.2 = hmm_par$emissionProbs.feel,
                                        sep = hmm_par$sep.hmm)




hmm_par$BMChofF.hmm.symbols = hmm_par$BMChofF$symbols
hmm_par$emissionProbs.BMChofF = hmm_par$BMChofF$matrix


rm(s)


