
global_path_Rdata = '/Users/laurasymul/Documents/Z_Rdump/Rdata/'
global_path_viz = '/Users/laurasymul/Documents/Z_Rdump/Rviz/'

res_folder = paste0(global_path_Rdata,'hmm_res/')
if(!dir.exists(res_folder)){dir.create(res_folder)}

viz_folder = paste0(global_path_viz,'hmm_res/')
if(!dir.exists(viz_folder)){dir.create(viz_folder)}


date.format = "%Y-%m-%d %H:%M:%S"

pdf.full.width = 16

#
colors_lsy = c(
  rgb(153,188,226, maxColorValue = 255),
  rgb(255,154,132, maxColorValue = 255),
  rgb(138,214,228, maxColorValue = 255),
  rgb(230,187,204, maxColorValue = 255),
  rgb(169,210,193, maxColorValue = 255),
  rgb(205,187,224, maxColorValue = 255),
  rgb(204,230,191, maxColorValue = 255),
  rgb(216,205,172, maxColorValue = 255),
  rgb(189,219,238, maxColorValue = 255),
  rgb(166,186,194, maxColorValue = 255))


colors_lsy_dark = c(
  hsv(rgb2hsv(col2rgb(colors_lsy[1]))[1],rgb2hsv(col2rgb(colors_lsy[1]))[2],rgb2hsv(col2rgb(colors_lsy[1]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[2]))[1],rgb2hsv(col2rgb(colors_lsy[2]))[2],rgb2hsv(col2rgb(colors_lsy[2]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[3]))[1],rgb2hsv(col2rgb(colors_lsy[3]))[2],rgb2hsv(col2rgb(colors_lsy[3]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[4]))[1],rgb2hsv(col2rgb(colors_lsy[4]))[2],rgb2hsv(col2rgb(colors_lsy[4]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[5]))[1],rgb2hsv(col2rgb(colors_lsy[5]))[2],rgb2hsv(col2rgb(colors_lsy[5]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[6]))[1],rgb2hsv(col2rgb(colors_lsy[6]))[2],rgb2hsv(col2rgb(colors_lsy[6]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[7]))[1],rgb2hsv(col2rgb(colors_lsy[7]))[2],rgb2hsv(col2rgb(colors_lsy[7]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[8]))[1],rgb2hsv(col2rgb(colors_lsy[8]))[2],rgb2hsv(col2rgb(colors_lsy[8]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[9]))[1],rgb2hsv(col2rgb(colors_lsy[9]))[2],rgb2hsv(col2rgb(colors_lsy[9]))[3]/1.5),
  hsv(rgb2hsv(col2rgb(colors_lsy[10]))[1],rgb2hsv(col2rgb(colors_lsy[10]))[2],rgb2hsv(col2rgb(colors_lsy[10]))[3]/1.5))


theme_lsy = theme_bw() +
  theme(text = element_text(size = 6)) + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "gray80",
                                        size = 0.5, linetype = "solid"))+
  theme(panel.border = element_blank())+
  theme(panel.grid.minor = element_line(size = 0.1)) + 
  theme(panel.grid.major = element_line(size = 0.3)) +
  theme(axis.ticks = element_line(size = 0.3))
theme_lsy = list(theme_lsy, scale_color_manual(values = colors_lsy))
update_geom_defaults("bar",   list(fill = colors_lsy[1]))



########################
# DICTIONARIES
types.fr = c('Prospecte', 'Cliente','Testeuse','Réference','Non Intéressée')
types.en = c('Prospect','Client','Test-user','Reference','Not interested')
types.index = 1:5
types.colors = colors_lsy[1:5]
types.dict = data.frame(index = types.index, types.fr = types.fr, types.en = types.en, colors = types.colors, stringsAsFactors = FALSE )

rm(types.fr, types.en, types.index, types.colors)

goals = c('Undefined', 'Conception','Observation','Contraception','Accept what comes')
goals.colors = c('gray',colors_lsy[3],colors_lsy[7],colors_lsy[2], colors_lsy[4])
goals.dict = data.frame(index = 0:4, goals = goals, colors = goals.colors, stringsAsFactors = FALSE )

rm(goals, goals.colors)

temp_method = c('Undefined','Oral','Anal','Vaginal')
temp_method.dict = data.frame(index = 0:3, temp_method = temp_method, stringsAsFactors = FALSE)

rm(temp_method)

billings = c('Undefined','Billings','SymptoThermal')
billings.short = c('-','B','ST')
billings.dict = data.frame(index = 0:2, billings = billings, billings.short = billings.short, stringsAsFactors = FALSE)

rm(billings, billings.short)

input_source = c('Sympto engine','user web','user app')
input_source.dict = data.frame(index = 0:2, input_source = input_source, stringsAsFactors = FALSE)

rm(input_source)


#sexual.intercourse = c('no entries', 'unprotected sex', 'protected sex')



#CYCLE PHASES

alpha = 0.4

pink = hsv(0.85,s = 0.3,v = 1)
pink.transp = hsv(0.85,s = 0.3, v = 1, alpha = alpha)
dark.pink = hsv(0.85,s = 0.5,v = 1)
dark.pink.transp = hsv(0.85,s = 0.5,v = 1, alpha = alpha)
light.blue = hsv(0.61,s = 0.3,v = 1)
light.blue.transp = hsv(0.61,s = 0.3,v = 1, alpha = alpha)
blue = hsv(0.61,s = 0.5,v = 1)
blue.transp = hsv(0.61,s = 0.5,v = 1, alpha = alpha)
yellow = hsv(0.12,s = 0.5,v = 1)
yellow.transp = hsv(0.12,s = 0.5,v = 1, alpha = alpha)
light.yellow = hsv(0.12, s = 0.3, v = 1)

dark.yellow = hsv(0.12,s = 0.8,v = 0.85)
gray = hsv(0,s=0,v = 0.5)
gray.transp = hsv(0,s=0,v = 0.5, alpha = alpha)


phases.numbers = 0:5
phases.names = c('Undefined',
                 'Infertile pre-ovulation',
                 'Fertile',
                 'Infertile post-ovulation',
                 'Very fertile',
                 'Breastfeeding')
phases.colors = c(gray.transp,
                  pink.transp,
                  light.blue.transp,
                  yellow.transp,
                  blue.transp,
                  dark.pink.transp)
phases.dict = data.frame(index =phases.numbers, names = phases.names, colors = phases.colors,stringsAsFactors = FALSE )

rm(phases.colors, phases.numbers, phases.names)

# BLEEDING
red.hue = 0.05
dark.red = hsv(red.hue, s = 1, v = 0.8)
medium.red = hsv(red.hue, s = 1, v = 1)
light.red = hsv(red.hue, s = 0.8, v = 1)
transparent = hsv(red.hue, s = 1, v = 0, alpha = 0)


source('HMM_menstrual_cycle.R')

bleeding.number = 0:3
bleeding.names = c('Nothing', 'small bleeding or spotting','medium bleeding','strong bleeding')
bleeding.colors = c(transparent, light.red, medium.red, dark.red)
bleeding.dict = data.frame(index = bleeding.number, 
                           names = bleeding.names, 
                           colors = bleeding.colors, 
                           hmm.symbols = bleeding.hmm.symbols[c(1,4,3,2)],
                           stringsAsFactors = FALSE)

rm(bleeding.number, bleeding.names, bleeding.colors)

# MUCUS
mucus.numbers = 0:4
mucus.names = c('Undefined', 'Eggwhite - little','Eggwhite - lots and stretchable','Sticky','Nothing')
mucus.colors = c(transparent, light.blue, blue, yellow, gray)
mucus.symbols = c(16,16,16,16,4)
mucus.cex = c(0,2,3,3,1)
mucus.dict = data.frame(index = mucus.numbers, 
                        names = mucus.names, 
                        colors = mucus.colors, 
                        symbols = mucus.symbols,
                        hmm.symbols = mucus.hmm.symbols[c(1,3,4,5,2)],
                        cex = mucus.cex,
                        stringsAsFactors = FALSE)

rm(mucus.numbers, mucus.names, mucus.colors, mucus.symbols, mucus.cex)

# CERVIX or FEELING
cervix.numbers = c(0:3,11:13)
cervix.names = c('Undefined','Dry','Wet','Very wet','Low and closed','Medium','High, soft and open')
cervix.colors = c(transparent,yellow,light.blue,blue, yellow,light.blue, blue)
cervix.symbols = c(16,17,17,17,1,1,1)
cervix.cex = c(0,0.5,1,2,0.5,1,2)
cervix.dict = data.frame(index = cervix.numbers, 
                         names = cervix.names, 
                         colors = cervix.colors, 
                         symbols = cervix.symbols,
                         cex = cervix.cex,
                         hmm.symbols = cervix.hmm.symbols[1:length(cervix.numbers)],
                         stringsAsFactors = FALSE)

rm(cervix.numbers, cervix.names, cervix.colors, cervix.symbols, cervix.cex)


# SEX
sex.red = hsv(0.97, s = 1, v = 1)
sex.numbers = 0:2
sex.names = c('Undefined', 'Unprotected Sex','Protected Sex')
sex.colors = c(transparent, sex.red, sex.red)
sex.bg = c(transparent, sex.red, transparent)
sex.symbols = c(25,25,1)
sex.dict = data.frame(index = sex.numbers, 
                      names = sex.names, 
                      colors = sex.colors,
                      bg = sex.bg,
                      symbols = sex.symbols,
                      stringsAsFactors = FALSE)

rm(sex.numbers, sex.names, sex.colors,sex.bg, sex.symbols)


# TEMP
margin.temp = 0.08

# gradient to show the variation in time
time.gradient = colorRampPalette(c('dodgerblue1','black','firebrick1'))
time.temp.gradient = time.gradient(15)

# STARS
stars.numbers = 0:4
stars.names = c('No star', 'Full star','Empty star','Full star deleted','Empty star deleted')
stars.symbols = c(21,23,23,21,21)
stars.col = c(transparent,'black','black',transparent, transparent)
stars.bg = c(transparent, 'black','white',transparent,transparent)
stars.dict = data.frame(index = stars.numbers, 
                        names = stars.names, 
                        colors = stars.col,
                        bg = stars.bg,
                        symbols = stars.symbols,
                        stringsAsFactors = FALSE)

rm(stars.numbers, stars.names, stars.symbols,stars.col, stars.bg)




# temperature
my_col = list()
my_col$cold = 'steelblue'
my_col$neutral = 'gray80'
my_col$warm = 'tomato'



temp.col.function <- colorRampPalette(c(my_col$cold, my_col$neutral, my_col$warm))

temp.values = seq(35.8,37, by = 0.1)
temp.colors = temp.col.function(length(temp.values))
temp.dict =  data.frame(values = temp.values, 
                          colors = temp.colors, 
                          stringsAsFactors = FALSE)

rm(temp.values, temp.colors)



# Jour Sommet PEAK DAY
js.color = hsv(0.25,s = 0.8, v = 0.6)
js.color.transp = hsv(0.25,s = 0.8, v = 0.6, alpha = 0.3)
js.numbers = 0:4
js.names = c('Not a peak day', 'Peak day','Premature peak day','Previous peak day','Cancelled peak day')
js.col = c(transparent,js.color,js.color.transp,js.color.transp,transparent)
js.dict = data.frame(index = js.numbers,
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
bf.col = c(transparent,bf.color,bf.color.transp,bf.color.transp,meno.color)
bf.dict = data.frame(index = bf.numbers,
                     names = bf.names,
                     colors = bf.col,
                     stringsAsFactors = FALSE)


rm(bf.color, bf.color.transp, meno.color,bf.numbers,bf.names, bf.col)

