


########################
# DICTIONARIES
types.fr = c('Prospecte', 'Cliente','Testeuse','Réference','Non Intéressée')
types.en = c('Prospect','Client','Test-user','Reference','Not interested')
types.index = 1:5
types.colors = viz_cols$colors_lsy[1:5]
dict$types = data.frame(index = types.index, types.fr = types.fr, types.en = types.en, colors = types.colors, stringsAsFactors = FALSE )
rm(types.fr, types.en, types.index, types.colors)

goals = c('Undefined', 'Conception','Observation','Contraception','Accept what comes')
goals.colors = c('gray',viz_cols$colors_lsy[3],viz_cols$colors_lsy[7],viz_cols$colors_lsy[2], viz_cols$colors_lsy[4])
dict$goals = data.frame(index = 0:4, goals = goals, colors = goals.colors, stringsAsFactors = FALSE )
rm(goals, goals.colors)

temp_method = c('Undefined','Oral','Anal','Vaginal')
dict$temp_method = data.frame(index = 0:3, temp_method = temp_method, stringsAsFactors = FALSE)
rm(temp_method)

billings = c('Undefined','Billings','SymptoThermal')
billings.short = c('-','B','ST')
dict$billings = data.frame(index = 0:2, billings = billings, billings.short = billings.short, stringsAsFactors = FALSE)
rm(billings, billings.short)

input_source = c('Sympto engine','user web','user app')
dict$input_source = data.frame(index = 0:2, input_source = input_source, stringsAsFactors = FALSE)
rm(input_source)


#sexual.intercourse = c('no entries', 'unprotected sex', 'protected sex')



#CYCLE PHASES

viz_cols$alpha = 0.4

viz_cols$pink = hsv(0.85,s = 0.3,v = 1)
viz_cols$pink.transp = hsv(0.85,s = 0.3, v = 1, alpha = viz_cols$alpha)
viz_cols$dark.pink = hsv(0.85,s = 0.5,v = 1)
viz_cols$dark.pink.transp = hsv(0.85,s = 0.5,v = 1, alpha = viz_cols$alpha)
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


phases.numbers = 0:5
phases.names = c('Undefined',
                 'Infertile pre-ovulation',
                 'Fertile',
                 'Infertile post-ovulation',
                 'Very fertile',
                 'Breastfeeding')
phases.colors = c(viz_cols$gray.transp,
                  viz_cols$pink.transp,
                  viz_cols$light.blue.transp,
                  viz_cols$yellow.transp,
                  viz_cols$blue.transp,
                  viz_cols$dark.pink.transp)
dict$phases = data.frame(index =phases.numbers, names = phases.names, colors = phases.colors,stringsAsFactors = FALSE )

rm(phases.colors, phases.numbers, phases.names)

# BLEEDING
viz_cols$red.hue = 0.05
viz_cols$dark.red = hsv(viz_cols$red.hue, s = 1, v = 0.8)
viz_cols$medium.red = hsv(viz_cols$red.hue, s = 1, v = 1)
viz_cols$light.red = hsv(viz_cols$red.hue, s = 0.8, v = 1)
viz_cols$very.light.red = hsv(viz_cols$red.hue, s = 0.5, v = 1)
viz_cols$transparent = hsv(viz_cols$red.hue, s = 1, v = 0, alpha = 0)


source('Scripts/HMM_menstrual_cycle_Sympto.R')

bleeding.number = c(0:3)
bleeding.names = c('Nothing', 'light bleeding or spotting','medium bleeding','heavy bleeding')
bleeding.colors = c(viz_cols$transparent, viz_cols$light.red, viz_cols$medium.red, viz_cols$dark.red)
dict$bleeding = data.frame(index = bleeding.number, 
                           names = bleeding.names, 
                           colors = bleeding.colors, 
                           hmm.symbols = hmm_par$bleeding.hmm.symbols[c(1,4,3,2)],
                           stringsAsFactors = FALSE)

rm(bleeding.number, bleeding.names, bleeding.colors)

# MUCUS
mucus.numbers = 0:4
mucus.names = c('Undefined', 'Eggwhite - little','Eggwhite - lots and stretchable','Sticky','Nothing')
mucus.colors = c(viz_cols$transparent, viz_cols$light.blue, viz_cols$blue, viz_cols$yellow, viz_cols$gray)
mucus.symbols = c(16,16,16,16,4)
mucus.cex = c(0,2,3,3,1)
dict$mucus = data.frame(index = mucus.numbers, 
                        names = mucus.names, 
                        colors = mucus.colors, 
                        symbols = mucus.symbols,
                        hmm.symbols = hmm_par$mucus.hmm.symbols[c(1,3,4,5,2)],
                        cex = mucus.cex,
                        stringsAsFactors = FALSE)

rm(mucus.numbers, mucus.names, mucus.colors, mucus.symbols, mucus.cex)

# CERVIX or FEELING
cervix.numbers = c(0:3,11:13)
cervix.names = c('Undefined','Dry','Wet','Very wet','Low and closed','Medium','High, soft and open')
cervix.colors = c(viz_cols$transparent,viz_cols$yellow,viz_cols$light.blue,viz_cols$blue, viz_cols$yellow,viz_cols$light.blue, viz_cols$blue)
cervix.symbols = c(16,17,17,17,1,1,1)
cervix.cex = c(0,0.5,1,2,0.5,1,2)
dict$cervix = data.frame(index = cervix.numbers, 
                         names = cervix.names, 
                         colors = cervix.colors, 
                         symbols = cervix.symbols,
                         cex = cervix.cex,
                         hmm.symbols = hmm_par$cervix.hmm.symbols[1:length(cervix.numbers)],
                         stringsAsFactors = FALSE)

rm(cervix.numbers, cervix.names, cervix.colors, cervix.symbols, cervix.cex)


# SEX
viz_cols$sex.red = hsv(0.97, s = 1, v = 1)
sex.numbers = 0:2
sex.names = c('Undefined', 'Unprotected Sex','Protected Sex')
sex.colors = c(viz_cols$transparent, viz_cols$sex.red, viz_cols$sex.red)
sex.bg = c(viz_cols$transparent, viz_cols$sex.red, viz_cols$transparent)
sex.symbols = c(25,25,1)
dict$sex = data.frame(index = sex.numbers, 
                      names = sex.names, 
                      colors = sex.colors,
                      bg = sex.bg,
                      symbols = sex.symbols,
                      stringsAsFactors = FALSE)

rm(sex.numbers, sex.names, sex.colors, sex.bg, sex.symbols)


# TEMP


# gradient to show the variation in time
viz_cols$time.gradient = colorRampPalette(c('dodgerblue1','black','firebrick1'))
viz_cols$time.temp.gradient = viz_cols$time.gradient(15)

# STARS
stars.numbers = 0:4
stars.names = c('No star', 'Full star','Empty star','Full star deleted','Empty star deleted')
stars.symbols = c(21,23,23,21,21)
stars.col = c(viz_cols$transparent,'black','black',viz_cols$transparent, viz_cols$transparent)
stars.bg = c(viz_cols$transparent, 'black','white',viz_cols$transparent,viz_cols$transparent)
dict$stars = data.frame(index = stars.numbers, 
                        names = stars.names, 
                        colors = stars.col,
                        bg = stars.bg,
                        symbols = stars.symbols,
                        stringsAsFactors = FALSE)

rm(stars.numbers, stars.names, stars.symbols,stars.col, stars.bg)




# temperature
 
viz_cols$cold = 'steelblue'
viz_cols$neutral = 'gray80'
viz_cols$warm = 'tomato'



viz_cols$temp.col.function <- colorRampPalette(c(viz_cols$cold, viz_cols$neutral, viz_cols$warm))

temp.values = seq(35.8,37, by = 0.1)
temp.colors = viz_cols$temp.col.function(length(temp.values))
dict$temp =  data.frame(values = temp.values, 
                          colors = temp.colors, 
                          stringsAsFactors = FALSE)

rm(temp.values, temp.colors)


# Jour Sommet PEAK DAY
js.color = hsv(0.25,s = 0.8, v = 0.6)
js.color.transp = hsv(0.25,s = 0.8, v = 0.6, alpha = 0.3)
js.numbers = 0:4
js.names = c('Not a peak day', 'Peak day','Premature peak day','Previous peak day','Cancelled peak day')
js.col = c(viz_cols$transparent,js.color,js.color.transp,js.color.transp,viz_cols$transparent)
dict$js = data.frame(index = js.numbers,
                     names = js.names,
                     colors = js.col,
                     stringsAsFactors = FALSE)

rm(js.color, js.color.transp, js.numbers,js.names, js.col)



# Breast feeding
bf.color = hsv(0,s = 0.8, v = 0.6)
bf.color.transp = hsv(0,s = 0.8, v = 0.6, alpha = 0.3)
meno.color = hsv(0.85, s = 1, v = 0.7)
bf.numbers = 0:4
bf.names = c('Nothing', '100% breastfeeding','Partial breastfeeding','Breastfeeding over','Pre-menopause')
bf.col = c(viz_cols$transparent,bf.color,bf.color.transp,bf.color.transp,meno.color)
dict$bf = data.frame(index = bf.numbers,
                     names = bf.names,
                     colors = bf.col,
                     stringsAsFactors = FALSE)


rm(bf.color, bf.color.transp, meno.color,bf.numbers,bf.names, bf.col)

