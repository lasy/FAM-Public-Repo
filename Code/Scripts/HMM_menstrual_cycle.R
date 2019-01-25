#### HMM

hmm_par = list()

hmm_par$sep.hmm = '_'

  

hmm_par$cycle.states = c('hM','lM','lE','hE','O','Rise','hP','Ep','lP','end')
hmm_par$cycle.states.colors = c(viz_cols$dark.red,viz_cols$light.red,viz_cols$light.blue,viz_cols$blue,'black','lightgoldenrod3',viz_cols$yellow,'darkseagreen3',viz_cols$light.yellow,'gray')

hmm_par$cycle.states.dict = data.frame(states=  hmm_par$cycle.states, colors = hmm_par$cycle.states.colors, stringsAsFactors = FALSE)


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
  eval(parse(text = paste0('hmm_par$cycle.states.transProbs = c(hmm_par$cycle.states.transProbs, cycle.states.transProbs.',s,')')))
}

hmm_par$cycle.states.transProbs = matrix(hmm_par$cycle.states.transProbs,10, byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cycle.states))

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(cycle.states.transProbs.',s,')')))
}


## MUCUS

hmm_par$mucus.hmm.symbols = c('NA','No','f','F','s','end')

###########################  NA   No   f    F    s    end
emissionProbs.mucus.hM   = c(0.45,0.4 ,0.05,0.05,0.05,0) 
emissionProbs.mucus.lM   = c(0.45,0.4 ,0.05,0.05,0.05,0) 
emissionProbs.mucus.lE   = c(0.35,0.35,0.25,0.05,0.05,0) 
emissionProbs.mucus.hE   = c(0.1 ,0.1 ,0.35,0.40,0.05,0) 
emissionProbs.mucus.O    = c(0.1 ,0.1 ,0.15,0.5 ,0.15,0) 
emissionProbs.mucus.Rise = c(0.1 ,0.1 ,0.3 ,0.2 ,0.3 ,0) 
emissionProbs.mucus.hP   = c(0.3 ,0.3 ,0.1 ,0.1 ,0.2 ,0) 
emissionProbs.mucus.Ep   = c(0.1 ,0.1 ,0.3 ,0.2 ,0.3 ,0) 
emissionProbs.mucus.lP   = c(0.4 ,0.4 ,0.05,0.05,0.1 ,0) 
emissionProbs.mucus.end  = c(0   ,0   ,0   ,0   ,0   ,1) 


hmm_par$emissionProbs.mucus = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.mucus = c(hmm_par$emissionProbs.mucus,emissionProbs.mucus.',s,')')))
}

hmm_par$emissionProbs.mucus = matrix(hmm_par$emissionProbs.mucus,
                             nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$mucus.hmm.symbols)
                             , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$mucus.hmm.symbols))

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.mucus.',s,')')))
}




## BLEEDING

hmm_par$bleeding.hmm.symbols = c('NA','3','2','1','end')

###########################     NA   3   2    1   end 
emissionProbs.bleeding.hM   = c(0.2 ,0.6 ,0.19,0.01,0) 
emissionProbs.bleeding.lM   = c(0.19,0.01,0.4 ,0.4 ,0) 
emissionProbs.bleeding.lE   = c(0.85,0.05,0.05,0.05,0) 
emissionProbs.bleeding.hE   = c(0.85,0.05,0.05,0.05,0) 
emissionProbs.bleeding.O    = c(0.70,0.05,0.05,0.20,0)
emissionProbs.bleeding.Rise = c(0.75,0.05,0.05,0.15,0) 
emissionProbs.bleeding.hP   = c(0.85,0.05,0.05,0.05,0) 
emissionProbs.bleeding.Ep   = c(0.85,0.05,0.05,0.05,0) 
emissionProbs.bleeding.lP   = c(0.60,0.05,0.05,0.3 ,0)
emissionProbs.bleeding.end  = c(0   ,0   ,0   ,0   ,1) 


hmm_par$emissionProbs.bleeding = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.bleeding = c(hmm_par$emissionProbs.bleeding,emissionProbs.bleeding.',s,')')))
}

hmm_par$emissionProbs.bleeding = matrix(hmm_par$emissionProbs.bleeding,
                                nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$bleeding.hmm.symbols)
                                , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$bleeding.hmm.symbols))


for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.bleeding.',s,')')))
}




## CERVIX + FEELING

hmm_par$cervix.hmm.symbols = c('NA','FD','FW','FVW','CC','CM','CH','end')

###########################   NA   FD   FW   FVW  CC   CM   CH  end 
emissionProbs.cervix.hM   = c(10  ,10  ,10  ,10  ,10  ,10  ,10 ,0) ; emissionProbs.cervix.hM = emissionProbs.cervix.hM/sum(emissionProbs.cervix.hM)
emissionProbs.cervix.lM   = c(10  ,10  ,10  ,10  ,10  ,10  ,10 ,0) ; emissionProbs.cervix.lM = emissionProbs.cervix.lM/sum(emissionProbs.cervix.lM)
emissionProbs.cervix.lE   = c(10  ,10  ,10  ,1   ,10  ,10  ,1 ,0) ; emissionProbs.cervix.lE = emissionProbs.cervix.lE/sum(emissionProbs.cervix.lE) 
emissionProbs.cervix.hE   = c(2   ,1   ,2   ,3   ,1   ,2   ,3  ,0) ; emissionProbs.cervix.hE = emissionProbs.cervix.hE/sum(emissionProbs.cervix.hE)
emissionProbs.cervix.O    = c(10  ,1   ,5   ,10  ,2   ,8   ,10 ,0) ; emissionProbs.cervix.O = emissionProbs.cervix.O/sum(emissionProbs.cervix.O)
emissionProbs.cervix.Rise = c(10  ,10  ,10  ,10  ,10  ,10  ,10 ,0) ; emissionProbs.cervix.Rise = emissionProbs.cervix.Rise/sum(emissionProbs.cervix.Rise)
emissionProbs.cervix.hP   = c(10  ,10  ,2   ,1   ,10  ,2   ,1  ,0) ; emissionProbs.cervix.hP = emissionProbs.cervix.hP/sum(emissionProbs.cervix.hP)
emissionProbs.cervix.Ep   = c(10  ,2   ,4   ,1   ,2   ,4   ,1  ,0) ; emissionProbs.cervix.Ep = emissionProbs.cervix.Ep/sum(emissionProbs.cervix.Ep) 
emissionProbs.cervix.lP   = c(10  ,8   ,2   ,1   ,8   ,2   ,1  ,0) ; emissionProbs.cervix.lP = emissionProbs.cervix.lP/sum(emissionProbs.cervix.lP)
emissionProbs.cervix.end  = c(0   ,0   ,0   ,0   ,0   ,0   ,0   ,1) 


hmm_par$emissionProbs.cervix = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.cervix = c(hmm_par$emissionProbs.cervix,emissionProbs.cervix.',s,')')))
}

hmm_par$emissionProbs.cervix = matrix(hmm_par$emissionProbs.cervix,
                              nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$cervix.hmm.symbols)
                              , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$cervix.hmm.symbols))

for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.',s,')')))
}




##############################
# COMBINATION BLEEDING + MUCUS

hmm_par$mucus.hmm.symbols
hmm_par$bleeding.hmm.symbols

hmm_par$emissionProbs.mucus
hmm_par$emissionProbs.bleeding


hmm_par$comb.hmm.symbols = as.vector(outer(hmm_par$mucus.hmm.symbols, hmm_par$bleeding.hmm.symbols, paste, sep=hmm_par$sep.hmm))

hmm_par$emissionProbs.comb = c()
for(si in 1:length(hmm_par$cycle.states)){
  emissionProbs.comb.si = as.vector(hmm_par$emissionProbs.mucus[si,] %*% t(hmm_par$emissionProbs.bleeding[si,]))
  hmm_par$emissionProbs.comb = c(hmm_par$emissionProbs.comb, emissionProbs.comb.si)
}

hmm_par$emissionProbs.comb

hmm_par$emissionProbs.comb = matrix(hmm_par$emissionProbs.comb,
                            nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$comb.hmm.symbols)
                            , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$comb.hmm.symbols))

rm(emissionProbs.comb.si)



##############################
# COMBINATION  MUCUS + BLEEDING + CERVIX&FEEL


hmm_par$comb.hmm.symbols
hmm_par$cervix.hmm.symbols

hmm_par$emissionProbs.comb
hmm_par$emissionProbs.cervix

hmm_par$MBC.hmm.symbols = as.vector(outer(hmm_par$comb.hmm.symbols, hmm_par$cervix.hmm.symbols, paste, sep=hmm_par$sep.hmm))

hmm_par$emissionProbs.MBC = c()
for(si in 1:length(hmm_par$cycle.states)){
  emissionProbs.MBC.si = as.vector(hmm_par$emissionProbs.comb[si,] %*% t(hmm_par$emissionProbs.cervix[si,]))
  hmm_par$emissionProbs.MBC = c(hmm_par$emissionProbs.MBC, emissionProbs.MBC.si)
}

hmm_par$emissionProbs.MBC

hmm_par$emissionProbs.MBC = matrix(hmm_par$emissionProbs.MBC,
                           nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$MBC.hmm.symbols)
                           , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$MBC.hmm.symbols))


rm(emissionProbs.MBC.si)



## FROM TEMP CURVE FITTING

hmm_par$temp.hmm.symbols = c('Foll','BOvu','Ovu', 'AOvu','Rise','High','Drop','end')
hmm_par$temp.hmm.colors = c(NA,'gray','black','gray','lightgoldenrod3',viz_cols$yellow,viz_cols$yellow.transp,'gray')


#######################     Foll BOvu Ovu  AOvu Rise High  Drop end 
emissionProbs.temp.hM   = c(10  ,1   ,1    ,1   ,1   ,1    ,1   ,0) 
emissionProbs.temp.lM   = c(10  ,2   ,1    ,1   ,1   ,1    ,1   ,0) 
emissionProbs.temp.lE   = c(10  ,5   ,2    ,1   ,1   ,1    ,1   ,0) 
emissionProbs.temp.hE   = c(10  ,10  ,5    ,1   ,1   ,1    ,1   ,0) 
emissionProbs.temp.O    = c(1   ,5   ,10   ,5   ,2   ,1    ,1   ,0) 
emissionProbs.temp.Rise = c(1   ,1   ,5    ,10  ,8   ,1    ,1   ,0) 
emissionProbs.temp.hP   = c(1   ,1   ,5    ,10  ,10  ,10   ,2   ,0) 
emissionProbs.temp.Ep   = c(1   ,1   ,1    ,2   ,3   ,5    ,3   ,0) 
emissionProbs.temp.lP   = c(1   ,1   ,1    ,1   ,1   ,1    ,10  ,0) 
emissionProbs.temp.end  = c(0   ,0   ,0    ,0   ,0   ,0    ,0   ,1) 


hmm_par$emissionProbs.temp = c()
for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('hmm_par$emissionProbs.temp = c(hmm_par$emissionProbs.temp,emissionProbs.temp.',s,')')))
}

hmm_par$emissionProbs.temp = matrix(hmm_par$emissionProbs.temp,
                                nrow = length(hmm_par$cycle.states), ncol = length(hmm_par$temp.hmm.symbols)
                                , byrow = TRUE, dimnames = list(hmm_par$cycle.states, hmm_par$temp.hmm.symbols))


hmm_par$emissionProbs.temp = hmm_par$emissionProbs.temp/apply(hmm_par$emissionProbs.temp,1,sum)


for(s in hmm_par$cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.temp.',s,')')))
}


rm(s, si)

