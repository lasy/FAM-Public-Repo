#### HMM

sep.hmm = '_'


cycle.states = c('hM','lM','lE','hE','O','Rise','hP','Ep','lP','end')
cycle.states.colors = c(dark.red,light.red,light.blue,blue,'black','lightgoldenrod3',yellow,'darkseagreen3',light.yellow,'gray')

cycle.states.dict = data.frame(states=  cycle.states, colors = cycle.states.colors, stringsAsFactors = FALSE)


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


cycle.states.transProbs = c()
for(s in cycle.states){
  eval(parse(text = paste0('cycle.states.transProbs = c(cycle.states.transProbs,cycle.states.transProbs.',s,')')))
}

cycle.states.transProbs = matrix(cycle.states.transProbs,10, byrow = TRUE, dimnames = list(cycle.states, cycle.states))

for(s in cycle.states){
  eval(parse(text = paste0('rm(cycle.states.transProbs.',s,')')))
}


## MUCUS

mucus.hmm.symbols = c('NA','No','f','F','s','end')

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


emissionProbs.mucus = c()
for(s in cycle.states){
  eval(parse(text = paste0('emissionProbs.mucus = c(emissionProbs.mucus,emissionProbs.mucus.',s,')')))
}

emissionProbs.mucus = matrix(emissionProbs.mucus,
                             nrow = length(cycle.states), ncol = length(mucus.hmm.symbols)
                             , byrow = TRUE, dimnames = list(cycle.states, mucus.hmm.symbols))

for(s in cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.mucus.',s,')')))
}




## BLEEDING

bleeding.hmm.symbols = c('NA','3','2','1','end')

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


emissionProbs.bleeding = c()
for(s in cycle.states){
  eval(parse(text = paste0('emissionProbs.bleeding = c(emissionProbs.bleeding,emissionProbs.bleeding.',s,')')))
}

emissionProbs.bleeding = matrix(emissionProbs.bleeding,
                                nrow = length(cycle.states), ncol = length(bleeding.hmm.symbols)
                                , byrow = TRUE, dimnames = list(cycle.states, bleeding.hmm.symbols))


for(s in cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.bleeding.',s,')')))
}




## CERVIX + FEELING

cervix.hmm.symbols = c('NA','FD','FW','FVW','CC','CM','CH','end')

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


emissionProbs.cervix = c()
for(s in cycle.states){
  eval(parse(text = paste0('emissionProbs.cervix = c(emissionProbs.cervix,emissionProbs.cervix.',s,')')))
}

emissionProbs.cervix = matrix(emissionProbs.cervix,
                              nrow = length(cycle.states), ncol = length(cervix.hmm.symbols)
                              , byrow = TRUE, dimnames = list(cycle.states, cervix.hmm.symbols))

for(s in cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.cervix.',s,')')))
}




##############################
# COMBINATION BLEEDING + MUCUS

mucus.hmm.symbols
bleeding.hmm.symbols

emissionProbs.mucus
emissionProbs.bleeding


comb.hmm.symbols = as.vector(outer(mucus.hmm.symbols, bleeding.hmm.symbols, paste, sep=sep.hmm))

emissionProbs.comb = c()
for(si in 1:length(cycle.states)){
  emissionProbs.comb.si = as.vector(emissionProbs.mucus[si,] %*% t(emissionProbs.bleeding[si,]))
  emissionProbs.comb = c(emissionProbs.comb, emissionProbs.comb.si)
}

emissionProbs.comb

emissionProbs.comb = matrix(emissionProbs.comb,
                            nrow = length(cycle.states), ncol = length(comb.hmm.symbols)
                            , byrow = TRUE, dimnames = list(cycle.states, comb.hmm.symbols))

rm(emissionProbs.comb.si)



##############################
# COMBINATION  MUCUS + BLEEDING + CERVIX&FEEL


comb.hmm.symbols
cervix.hmm.symbols

emissionProbs.comb
emissionProbs.cervix

MBC.hmm.symbols = as.vector(outer(comb.hmm.symbols, cervix.hmm.symbols, paste, sep=sep.hmm))

emissionProbs.MBC = c()
for(si in 1:length(cycle.states)){
  emissionProbs.MBC.si = as.vector(emissionProbs.comb[si,] %*% t(emissionProbs.cervix[si,]))
  emissionProbs.MBC = c(emissionProbs.MBC, emissionProbs.MBC.si)
}

emissionProbs.MBC

emissionProbs.MBC = matrix(emissionProbs.MBC,
                           nrow = length(cycle.states), ncol = length(MBC.hmm.symbols)
                           , byrow = TRUE, dimnames = list(cycle.states, MBC.hmm.symbols))


rm(emissionProbs.MBC.si)



## FROM TEMP CURVE FITTING

temp.hmm.symbols = c('Foll','BOvu','Ovu', 'AOvu','Rise','High','Drop','end')
temp.hmm.colors = c(NA,'gray','black','gray','lightgoldenrod3',yellow,yellow.transp,'gray')


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


emissionProbs.temp = c()
for(s in cycle.states){
  eval(parse(text = paste0('emissionProbs.temp = c(emissionProbs.temp,emissionProbs.temp.',s,')')))
}

emissionProbs.temp = matrix(emissionProbs.temp,
                                nrow = length(cycle.states), ncol = length(temp.hmm.symbols)
                                , byrow = TRUE, dimnames = list(cycle.states, temp.hmm.symbols))


emissionProbs.temp = emissionProbs.temp/apply(emissionProbs.temp,1,sum)


for(s in cycle.states){
  eval(parse(text = paste0('rm(emissionProbs.temp.',s,')')))
}

