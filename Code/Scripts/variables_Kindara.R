par$n_cores = detectCores() - 1

viz$pos.y.labels = 1 - 0.3




# COLORS

viz_cols$transparent = hsv(h = 0, s = 1, v = 0, alpha = 0)
viz_cols$alpha = 0.4

# sex
viz_cols$sex.red = hsv(0.97, s = 1, v = 1)

# bleeding
viz_cols$red.hue = 0.05 
viz_cols$dark.red = hsv(viz_cols$red.hue, s = 1, v = 0.8)
viz_cols$medium.red = hsv(viz_cols$red.hue, s = 1, v = 1)
viz_cols$light.red = hsv(viz_cols$red.hue, s = 0.8, v = 1)
viz_cols$very.light.red = hsv(viz_cols$red.hue, s = 0.5, v = 1)


# mucus
viz_cols$light.creamy = hsv(0.2,s = 0.3,v = 0.9)
viz_cols$light.creamy.transp = hsv(0.2,s = 0.3,v = 0.9, alpha = viz_cols$alpha)
viz_cols$light.blue = hsv(0.61,s = 0.3,v = 1)
viz_cols$light.blue.transp = hsv(0.61,s = 0.3,v = 1, alpha = viz_cols$alpha)
viz_cols$blue = hsv(0.61,s = 0.5,v = 1)
viz_cols$blue.transp = hsv(0.61,s = 0.5,v = 1, alpha = viz_cols$alpha)
viz_cols$yellow = hsv(0.12,s = 0.5,v = 1)
viz_cols$yellow.transp = hsv(0.12,s = 0.5,v = 1, alpha = viz_cols$alpha)
viz_cols$light.yellow = hsv(0.12, s = 0.3, v = 1)
viz_cols$dark.yellow = hsv(0.12,s = 0.8,v = 0.85)
viz_cols$gray = hsv(0,s=0,v = 0.5)
viz_cols$gray.transp = hsv(0,s=0,v = 0.5, alpha = viz_cols$alpha)


# temperature
viz_cols$cold = 'steelblue'
viz_cols$neutral = 'gray80'
viz_cols$warm = 'tomato'


source('Scripts/HMM_menstrual_cycle_Kindara.R')


# SEX

sex.index = 0:4
sex.names = c('No sex','Protected sex','Unprotected sex','Withdrawal','Insemination')
sex.short.names = c('no_sex','prot_sex','unprot_sex','withdrawal','insemination')
sex.colors = c(viz_cols$transparent, viz_cols$sex.red, viz_cols$sex.red, viz_cols$sex.red,'black')
sex.bg = c(viz_cols$transparent, viz_cols$transparent, viz_cols$sex.red, NA, NA)
sex.symbols = c(1,23,23,9,8)
dict$sex = data.frame(index = sex.index, 
                      names = sex.names, 
                      short.names =sex.short.names, 
                      symbols = sex.symbols,
                      colors  = sex.colors,
                      bg = sex.bg,
                      stringsAsFactors = FALSE)

rm(sex.index, sex.names,sex.short.names,sex.colors,sex.bg,sex.symbols)


#GOAL
goal.index = 0:5
goal.names = c('Undefined','Conception','Observation','Contraception','Accept what comes','Unknown')
goal.short.names = c('undef','conc','obs','contra','whatever','unknown')
dict$goal = data.frame(index = goal.index, names = goal.names, short.names =goal.short.names, stringsAsFactors = FALSE)
rm(goal.index, goal.names,goal.short.names)



# PRESUMED GOAL
pres.goal.index = 0:7
pres.goal.names = c('Undefined','Conception','Observation','Contraception','Accept what comes','Unknown', 'Insemination','Withdrawal')
pres.goal.short.names = c('undef','conc','obs','contra','whatever','unknown','insem','withdrawal')
pres.goal.colors = c('gray','pink','orange','steelblue','green3','gray80','black','purple')
dict$pres.goal = data.frame(index = pres.goal.index, 
                            names = pres.goal.names, 
                            short.names = pres.goal.short.names, 
                            colors = pres.goal.colors,
                            stringsAsFactors = FALSE)
rm(pres.goal.index, pres.goal.names,pres.goal.short.names,pres.goal.colors)




# TEST


preg.test.index = 0:2
preg.test.names = c('Undefined','Positive','Negative')
preg.test.colors = c(viz_cols$transparent, 'steelblue', 'red4')
preg.test.pch = c(NA, 16, 4)
dict$preg.test = data.frame(index = preg.test.index, 
                            names = preg.test.names, 
                            colors = preg.test.colors,
                            pch = preg.test.pch, 
                            stringsAsFactors = FALSE)
rm(preg.test.index, preg.test.names,preg.test.colors,preg.test.pch)


opk.index = 0:2
opk.names = c('Undefined','Positive','Negative')
opk.colors = c(viz_cols$transparent, 'steelblue', 'red4')
opk.pch = c(NA, 16, 4)
dict$opk = data.frame(index = opk.index, names = opk.names, 
                      colors = opk.colors,
                      pch = opk.pch, 
                      stringsAsFactors = FALSE)
rm(opk.index, opk.names,opk.colors,opk.pch)



# BLEEDING

bleeding.index = sort(c(0.5,0:3))
bleeding.names = c('No bleeding','Spotting','Light bleeding','Medium bleeding','Heavy bleeding')
bleeding.colors = c(viz_cols$transparent,viz_cols$very.light.red, viz_cols$light.red,viz_cols$medium.red,viz_cols$dark.red)
bleeding.pch = c(1,1,16,16,16)

dict$bleeding = data.frame(index = bleeding.index, 
                           names = bleeding.names, 
                           colors = bleeding.colors, 
                           pch = bleeding.pch,
                           hmm.symbols = hmm_par$bleeding.hmm.symbols[c(1,5,4,3,2)],
                           stringsAsFactors = FALSE)

rm(bleeding.pch, bleeding.colors, bleeding.names, bleeding.index )


# MUCUS


quantities = c('little','medium','lots')
mucus.numbers = c(NA,0:12)
mucus.names = c('Undefined','Nothing', 
                paste('Sticky -',quantities), 
                paste('Creamy -',quantities), 
                paste('Eggwhite -',quantities), 
                paste('Watery -',quantities))
mucus.colors = c(viz_cols$transparent, viz_cols$gray, 
                 rep(viz_cols$yellow,3), 
                 rep(viz_cols$light.creamy,3), 
                 rep(viz_cols$light.blue,2), viz_cols$blue , 
                 viz_cols$light.blue, rep(viz_cols$blue,2))
mucus.symbols = c(16,4,rep(16,9),rep(15,3))
mucus.cex = c(0,1,1:3,1:3,1:3,1:3)
mucus.lty = c(rep(1,11),rep(2,3))
dict$mucus = data.frame(index = mucus.numbers, 
                        names = mucus.names, 
                        colors = mucus.colors, 
                        symbols = mucus.symbols,
                        hmm.symbols = hmm_par$mucus.hmm.symbols[1:length(mucus.numbers)],
                        cex = mucus.cex,
                        lty = mucus.lty,
                        stringsAsFactors = FALSE)

rm(mucus.numbers, mucus.names, mucus.colors, mucus.symbols, mucus.cex,mucus.lty, quantities)


# FEEL


feel.numbers = c(NA,0:4)
feel.names = c('Undefined','Nothing','Dry','Dry sticky','Wet moist','Wet lubricative')
feel.colors = c(viz_cols$transparent, viz_cols$transparent, viz_cols$light.creamy, viz_cols$yellow, viz_cols$light.blue,viz_cols$blue)
feel.symbols = rep(16,6)
dict$feel = data.frame(index = feel.numbers, 
                        names = feel.names, 
                        colors = feel.colors, 
                        symbols = feel.symbols,
                        hmm.symbols = hmm_par$feel.hmm.symbols[c(1,1:5)],
                        stringsAsFactors = FALSE)

rm(feel.numbers, feel.names, feel.colors, feel.symbols)




# CERVIX

#firmness
firmness.numbers = c(0:3)
firmness.names = c('Undefined','Firm','Medium','Soft')
firmness.colors = c('gray80', viz_cols$yellow, viz_cols$light.blue, viz_cols$blue)
dict$firmness = data.frame(index = firmness.numbers, 
                        names = firmness.names, 
                        colors = firmness.colors, 
                        hmm.symbols = hmm_par$cervix.firmness.hmm.symbols[1:4],
                        stringsAsFactors = FALSE)

rm(firmness.numbers, firmness.names, firmness.colors)



#openness
openness.numbers = c(0:3)
openness.names = c('Undefined','Closed','Medium','Open')
openness.colors = c('gray80', viz_cols$yellow, viz_cols$light.blue, viz_cols$blue)
dict$openness = data.frame(index = openness.numbers, 
                           names = openness.names, 
                           colors = openness.colors, 
                           hmm.symbols = hmm_par$cervix.openness.hmm.symbols[1:4],
                           stringsAsFactors = FALSE)

rm(openness.numbers, openness.names, openness.colors)



#height
height.numbers = c(0:3)
height.names = c('Undefined','Low','Medium','High')
height.colors = c('gray80', viz_cols$yellow, viz_cols$light.blue, viz_cols$blue)
dict$height = data.frame(index = height.numbers, 
                           names = height.names, 
                           colors = height.colors, 
                         hmm.symbols = hmm_par$cervix.height.hmm.symbols[1:4],
                         stringsAsFactors = FALSE)

rm(height.numbers, height.names, height.colors)


# CERVIX (max of any of the 3)

cervix.numbers = c(0:3)
cervix.names = c('Undefined','Low fertility','Medium','High fertility')
cervix.colors = c(viz_cols$transparent, viz_cols$yellow, viz_cols$light.blue, viz_cols$blue )
dict$cervix =  data.frame(index = cervix.numbers, 
                          names = cervix.names, 
                          colors = cervix.colors, 
                          hmm.symbols = hmm_par$cervix.hmm.symbols[1:4],
                          stringsAsFactors = FALSE)

rm(cervix.numbers, cervix.names, cervix.colors)



# TEMPERATURE (max of any of the 3)

temp.col.function <- colorRampPalette(c(viz_cols$cold, viz_cols$neutral, viz_cols$warm))

temp.values = seq(96,99, by = 0.5)
temp.colors = temp.col.function(length(temp.values))
dict$temp =  data.frame(values = temp.values, 
                          colors = temp.colors, 
                          stringsAsFactors = FALSE)

rm(temp.values, temp.colors)





# CLEAN WORKSPACE




