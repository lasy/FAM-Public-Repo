
##############################
# plotting function


plot_tracking_freq = function(cycles, goal, sex){
  j = (cycles$goal == goal)
  if(sex){j = j&(cycles$has_logged_sex >0)}else{j = j& (cycles$has_logged_sex==0)}
  #cat(length(which(j)),'\n')
  x = cycles$tracking_freq[j]
  h = hist(x, breaks = breaks, plot = FALSE)
  f = h$counts/sum(h$counts)
  at.x = seq(0,1,by = 0.1)
  at.y = seq(0,0.5,by = 0.1)
  plot(h$mids,f, type = 'n', axes = FALSE, xlim = c(0,1), ylim = range(0,0.45),yaxs = 'i', xaxs = 'i',
       xlab = 'Tracking frequency (#day with obs. / cycle length)', ylab = '% of cycles', 
       main = paste(dict$goals$goals[dict$goals$index ==  goal],'|', sex,'|',length(x)))
  axis(1, col = 'lightgray', col.ticks = 'lightgray')
  axis(2, at = at.y , labels = 100*at.y, col = 'lightgray', col.ticks = 'lightgray')
  abline(h = at.y, col = 'lightgray')
  abline(v = at.x, col = 'lightgray')
  rect(xleft = breaks[1:(N-1)],ybottom = rep(0,N-1),xright = breaks[2:N],ytop = f, col = dict$goals$colors[dict$goals$index == goal], border = NA)
  med = median(x, na.rm = TRUE)
  abline(v = med, lty = 2)
  text(x = med, y = at.y[2]/3, pos = 4, labels = round(med,digits = 2))
  tfh = data.frame(tracking_freq = h$mid, fraction_of_cycles = f)
  return(tfh)
}




plot_menarque_hist = function(users){
  j = 1:nrow(users)
  x = users$menarche_year[j]
  h = hist(x, breaks = breaks, plot = FALSE)
  f = h$counts/sum(h$counts)
  at.x = seq(min(h$mid),max(h$mid),by = 1)
  at.y = seq(round(min(f)),0.5,by = 0.1)
  plot(range(h$breaks),range(f), type = 'n', axes = FALSE, xlim = range(h$breaks), ylim = range(0,0.3),yaxs = 'i', xaxs = 'i',
       xlab = 'Menarche age', ylab = '% of users')
  axis(1, col = 'lightgray', col.ticks = 'lightgray')
  axis(2, at = at.y , labels = 100*at.y, col = 'lightgray', col.ticks = 'lightgray')
  abline(h = at.y, col = 'lightgray')
  abline(v = at.x, col = 'lightgray')
  rect(breaks[1:(N-1)],rep(0,N-1),breaks[2:N],f, col = colors_lsy[1], border = colors_lsy_dark[1])
  med = median(x, na.rm = TRUE)
  mn = mean(x, na.rm = TRUE)
  abline(v = mn, lty = 2, lwd =1)
  cat('median',med,'\n')
  cat('average',mn,'\n')
  text(x = mn, y = at.y[2]/3, pos = 4, labels = round(mn,digits = 2))
}




plot_menarche_hist_split_gg = function(users, split = NA){
  data = data.frame(menarche_year = users$menarche_year, split = factor(x = split, levels = sort(unique(split))))
  ggplot(data, aes(x = menarche_year, y = split)) +
    geom_density_ridges(scale = 4) + theme_ridges() +
    scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0))      # for both axes to remove unneeded padding
  #> Picking joint bandwidth of 458
}

plot_hist_split = function(values, breaks = breaks, split = NA, max.freq = 0.3){
  data = data.frame(values = values, split = factor(x = split, levels = sort(unique(split))))
  lev.split = levels(data$split)
  N.split = length(lev.split)
  x = data$values
  h = hist(x, breaks = breaks, plot = FALSE)
  
  at.y = seq(0,max.freq,by = 0.2)
  plot(range(breaks), c(0,N.split), type = 'n', axes = FALSE, yaxs = 'i', xaxs = 'i',
       xlab = 'Menarche age', ylab = '% of users')
  axis(1, col = 'lightgray', col.ticks = 'lightgray')
  y.axis = rep(1:N.split,each = length(at.y)-1)-1 + seq(0,1,len = length(at.y))[1:(length(at.y)-1)]
  axis(2, at = y.axis , labels = rep(100*at.y[1:(length(at.y)-1)],N.split), col = 'lightgray', col.ticks = 'lightgray')
  axis(4, at = 1:N.split- 0.5, labels = lev.split,tick = FALSE, col = 'lightgray', col.ticks = 'lightgray')
  abline(h = y.axis, col = 'lightgray')
  abline(v = h$mids, col = 'lightgray')
  
  for(i in 1:N.split){
    s = lev.split[i]
    x = data$values[data$split == s]
    h = hist(x, breaks = breaks, plot = FALSE)
    f = h$counts/sum(h$counts)
    rect(breaks[1:(N-1)],rep(i-1,N-1),breaks[2:N],f/max.freq+i-1, col = colors_lsy[1], border = colors_lsy_dark[1])
    med = median(x, na.rm = TRUE)
    mn = mean(x, na.rm = TRUE)
    segments(x0 = mn, x1 = mn, y0 = i-1, y1 = i , lty = 3, lwd = 2)
    
    cat('median',med,'\n')
    cat('average',mn,'\n')
    text(x = mn, y = i-0.2, pos = 4, labels = round(mn,digits = 2), cex = 0.7)
  }
  
}


plot_hist = function(x, bin.size = 1, 
                     at.x = NA, at.y = NA, 
                     xlab = '', ylab = '',
                     xlim = c('at.x','all.values'),
                     col = 'gray50', border = 'gray40',
                     plot.median = TRUE, bars = TRUE){
  
  min = floor(min(x, na.rm = TRUE))
  max = ceiling(max(x, na.rm = TRUE))
  breaks = seq(min,max+bin.size, by = bin.size)-0.5*bin.size
  N = length(breaks)
  h = hist(x, breaks = breaks, plot = FALSE)
  f = h$counts/sum(h$counts)
  h$f = f
  if(is.na(at.x[1])){at.x = seq(min, max,by = round(200*((max - min)/10))/200) }
  if(is.na(at.y[1])){at.y = seq(0, max(f)*1.1,by = round(200*(max(f)/10))/200) }
  
  if(xlim[1] == 'at.x'){xlim = range(at.x)}else{xlim = range(breaks)}
  
  plot(h$mids,f, type = 'n', 
       axes = FALSE, xlim = xlim, ylim = range(at.y),
       yaxs = 'i', xaxs = 'i',
       xlab = xlab, ylab = ylab)
  axis(1, at = at.x , col = 'lightgray', col.ticks = 'lightgray', las = 1)
  axis(2, at = at.y , labels = 100*at.y, col = 'lightgray', col.ticks = 'lightgray', las = 1)
  abline(h = at.y, col = 'lightgray')
  abline(v = at.x, col = 'lightgray')
  if(bars){rect(xright =breaks[1:(N-1)],ybottom = rep(0,N-1),xleft = breaks[2:N], ytop = f,col = col, border = border)}else{
  	points(h$mids, f, type = 'l', col = border); points(h$mids, f, type = 'p', col = border, cex = 0.5, pch = 16)}
  if(plot.median){
    med = median(x, na.rm = TRUE)
    if(bars){col.med = 'black'}else{col.med = border} 
    abline(v = med, lty = 2, col = col.med)
    #text(x = med, y = at.y[2]/3, pos = 2, labels = round(med,digits = 2))
  }
  return(h)
}






###############################
# plot cycles



plot_cycles = function(cycletable, 
                       #abo_id = c(42782,32807), 
                       #cycle_id = c('42782_4','42782_5','32807_4','32807_5'),
                       file_path = 'viz/cycles/', filename_prefix = '',filename_suffix = ''){
  
  
  source('Variables.R')
  # TO DO LATER: select the correct rows from the cycletable
  # For now, we assume we need to plot all rows from the cycletable
  
  # first we split by individual abos
  abos = unique(cycletable$user_id)
  n.abo = length(abos)
  
  for(abo in abos){
    
    subtable = cycletable[cycletable$user_id == abo,]
    cycle_nbs = unique(subtable$cycle_nb)
    
    n.cycles = length(cycle_nbs)
    # we create one pdf per abo
    if(filename_prefix == ''){prefix = ''}else{prefix = paste0(filename_prefix,'_')}
    if(filename_suffix == ''){suffix = ''}else{suffix = paste0(filename_prefix,'_')}
    
    if(n.cycles <= 10){cycles_main = paste0(cycle_nbs,collapse = '-')}else{cycles_main = paste0(min(cycle_nbs),'-',max(cycle_nbs))}
    
    filename = paste0(file_path,prefix,
                      'abo_',abo,
                      '_cycles_', cycles_main,'_',
                      suffix, gsub(':','-',now()),'.pdf')
    cat(filename,"\n")
    ################
    ################
    ################
    pdf(file = filename, width = 7*max(subtable$out_cycleday)/30, height = 2*n.cycles, useDingbats = FALSE)
    source('Variables.R')
    
    #layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(3,1), heights=c(1,2))
    par(mfrow = c(n.cycles,1), cex = 0.5, las = 1)
    
    #left justified plots (aligned on first day of period)
    for(cycle_nb in cycle_nbs){
      cat(cycle_nb,'\n')
      plot_cycle(subtable = subtable, cycle_nb = cycle_nb, abo = abo)
      
    }
    
    dev.off()
    ################
    ################
    ################
  }
}


################
# PLOT an individual cycle
################

plot_cycle = function(subtable = subtable, 
#                      cycles = cycles, 
                      cycle_nb = NA, 
                      abo = '0', 
                      app = "Sympto",
                      xaxis.top = FALSE, 
                      xaxis.bottom = TRUE, 
                      show_phases = FALSE, 
                      show_weights = FALSE,
                      show_stars = TRUE){
  
 
  if(app == "Kindara"){
    subtable$cycle_nb = subtable$cycle_number
    subtable$out_cycleday = subtable$cycleday
    subtable$blood = subtable$bleeding}
  else{
    subtable$mucus = subtable$elixir
  }
  
  if(is.na(cycle_nb)){ cycle_nb = unique(subtable$cycle_nb)[1]}
  
  if(abo == '0'){ abo = unique(subtable$user_id)[1]}
  
  k = which(subtable$cycle_nb == cycle_nb)
  this.cycle = subtable[k,]
  
  cycle_id = unique(subtable$cycle_id)[1]
  
  if(app == "Sympto"){goal = unique(this.cycle$goal)}else{goal = "Not defined"}
  #cat(goal,'\n')
  if(app == "Sympto"){mode = unique(this.cycle$method)}else{mode = "Tracking only"}
  #cat(mode,'\n')
  
  
  
  n.lines = 5
  par(las = 1, mar = c(1, 2.5, 4, 0.5) + 0.1)
  main = paste0('ID: ',abo, ' | Cycle: ',cycle_nb,' | goal: ',goal,'  |  mode: ',mode,'\n')
  xlim = range(c(0,subtable$out_cycleday))
  n.days = xlim[2]

  ylim = c(-n.lines/10-0.05,1)
  plot(range(c(0,subtable$out_cycleday)),c(-n.lines/10-0.05,1),type = 'n', 
       main = main, xlab = '',ylab='', adj = 0,
       axes = FALSE, xlim = xlim, ylim = ylim)
  #phases & grid
  for(d in this.cycle$out_cycleday){
    #grid (vertical grid)
    if(d < max(this.cycle$out_cycleday)){segments(x0 = d+0.5,y0 = -n.lines-1, x1 = d+0.5,y1 = 1-show_phases, col = 'gray90')}
    #phase
    if(show_phases){
      this.day = this.cycle[this.cycle$out_cycleday == d,]
      col = phases.dict$colors[match(this.day$out_phase ,phases.dict$index)]
      rect(d-0.5,0,d+0.5,1,col = col, border = 'white')
    }
  }
  #bleeding
  line.blood = -0.1
  points(this.cycle$out_cycleday, rep(line.blood,n.days), 
         pch = 16, cex = this.cycle$blood, 
         col = dict$bleeding$colors[match(this.cycle$blood,dict$bleeding$index)])
  axis(2, at = line.blood, label = 'bleeding',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  
  
  #cervical mucus
  line.mucus = -0.2
  points(this.cycle$out_cycleday, rep(line.mucus,n.days), 
         pch = dict$mucus$symbols[match(this.cycle$mucus,dict$mucus$index)], 
         cex = dict$mucus$cex[match(this.cycle$mucus,dict$mucus$index)], 
         col = dict$mucus$colors[match(this.cycle$mucus,dict$mucus$index)])
  axis(2, at = line.mucus, label = 'mucus',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  
  
  # cervix position or vaginal feeling
  line.cervix = -0.35
  points(this.cycle$out_cycleday, rep(line.cervix, n.days),
         pch = dict$cervix$symbols[match(this.cycle$feel,dict$cervix$index)],
         cex = dict$cervix$cex[match(this.cycle$feel,dict$cervix$index)],
         col = dict$cervix$colors[match(this.cycle$feel,dict$cervix$index)])
  if(max(this.cycle$feel, na.rm = TRUE) %in% c(0,11,12,13)){label.cervix = 'cervix'} else{label.cervix = 'feel'}
  axis(2, at = line.cervix, label = label.cervix,tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  
  
  #sex
  line.sex = -0.47
  points(this.cycle$out_cycleday, rep(line.sex,n.days), 
         pch = 25, cex = 2, 
         col = dict$sex$colors[match(this.cycle$sex,dict$sex$index)], 
         bg = dict$sex$bg[match(this.cycle$sex,dict$sex$index)])
  axis(2, at = line.sex, label = 'sex',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  
  
  #temperature
  days.with.tmp = this.cycle$out_cycleday[!is.na(this.cycle$temp_c)]
  if(length(days.with.tmp)>0){
    tct = this.cycle[this.cycle$out_cycleday %in% days.with.tmp,]
    rt = range(tct$temp_c, na.rm = TRUE)
    if(rt[2] <= (rt[1]+0.2)){rt[1] = rt[1] - 0.05; rt[2] = rt[2] + 0.05}
    
    tct$adj.temp = adj.tmp.fn(tct$temp_c,rt,viz$margin.temp)
    
    grid.value = seq(0.1*ceiling(10*rt[1]),0.1*floor(10*rt[2]),by = 0.1)
    adj.grid.value = adj.tmp.fn(grid.value, rt, viz$margin.temp)
    n.grid = length(adj.grid.value)
    
    if(show_phases){col.grid = 'white'}else{col.grid = 'gray90'}
    #abline(h = adj.grid.value , col = col.grid)
    segments(x0 = rep(min(this.cycle$out_cycleday)-0.5,n.grid), 
             x1 = rep(max(this.cycle$out_cycleday)+0.5,n.grid),
             y0 = adj.grid.value, y1 = adj.grid.value,
             col = col.grid)
    
    # plotting temperature + time indication
    it = which(diff(tct$out_cycleday) ==1)
    for(i in it){segments(tct$out_cycleday[i],tct$adj.temp[i],
                          tct$out_cycleday[i+1],tct$adj.temp[i+1],lwd = 0.5)}
    
    
    if(show_weights){
      weights = compute_weights(cycletable = tct)
      col.t = paste0('gray',100-round(100*weights))
    }else{col.t = 'black'}
    
    points(tct$out_cycleday, tct$adj.temp, type = 'p', pch = 16, cex = 1, col = col.t)
    text(tct$out_cycleday, tct$adj.temp, labels = as.character(tct$time_temp), pos = 3, cex = 0.5)
    
    
    #stars
    if(show_stars){
    points(tct$out_cycleday,  tct$adj.temp, type = 'p',
           pch = stars.dict$symbols[match(tct$out_star,stars.dict$index)], cex = 1.7, 
           col = stars.dict$colors[match(tct$out_star,stars.dict$index)], 
           bg = stars.dict$bg[match(tct$out_star,stars.dict$index)])
    }
    
  }else{adj.grid.value = c(0.5); grid.value = c(NA); rt = c(0,0)}
  
  #peak day (jour sommet)
  if(show_phases){pch.js = 24}else{pch.js = 25}
  points(this.cycle$out_cycleday+0.5,rep(0,n.days),
         pch = pch.js,cex = 2, col =NA,
         bg = dict$js$colors[match(this.cycle$out_js,dict$js$index)])
  
  #breast feeding
  points(this.cycle$out_cycleday+0.5,rep(0,n.days),
         pch = 17,cex = 2,
         col = dict$bf$colors[match(this.cycle$bf,dict$bf$index)])
  
  #axes
  min.d = min(this.cycle$out_cycleday)
  max.d = max(this.cycle$out_cycleday)
  if(xaxis.bottom){
    axis(1,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
    axis(1,at = this.cycle$out_cycleday, tick = FALSE, padj = -1)
  }
  if(xaxis.top){
    axis(3,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
    axis(3,at = this.cycle$out_cycleday, tick = FALSE, padj = 1)
  }
  axis(2, at = adj.grid.value, labels = grid.value, las = 1, lwd = 0, lwd.tick = 1, pos = min(this.cycle$out_cycleday)- 0.6)
  
  return(list(rt= rt, margin.temp = viz$margin.temp))
}





adj.tmp.fn = function(t,rt,margin.temp){
  if(rt[1]<rt[2]){
    (t - rt[1])/(rt[2]-rt[1])*(1-2*margin.temp)+margin.temp
  }else{0.5}
}

#######
#######

plot_cycle_with_ovu_detection = function(cycletable = cycletable, opt = opt, publi = TRUE ){
  
  if(publi){
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
           widths=c(1,1), heights=c(1,1.3))
    par(cex.main = 0.9, cex=0.8)
  }else{par(mfrow = c(4,1))}
  
  plot_cycle(subtable = cycletable)
  
  plot_fit_results(cycletable = cycletable, opt = opt, publi = publi)
  
}


plot_fit_results = function(cycletable = cycletable, opt = opt, publi = TRUE) {
  
  this.cycle = cycletable
  scale.hmm = 0.6
  if(publi){n.lines = 1.25}else{n.lines = 3}
  lh = 0.2
  
  xlim = range(c(0,cycletable$out_cycleday))
  ylim = c(-n.lines*lh-publi*scale.hmm,1)
  adj.grid.value = c(0,0.5,1)
  grid.value =  rep('',3) 
  if(opt$do.temp.fit){
    main = paste0(
      paste0(names(opt$opt$par),': ',round(opt$opt$par,digits = 2)),
      collapse = '  |  ')
  }else{main = 'temperature not taken into account'}
  
  if(publi){par(las = 1, mar = c(2, 2.5, 3.5, 0.5) + 0.1)}else{par(las = 1, mar = c(1, 2.5, 3.5, 0.5) + 0.1)}
  plot(xlim,ylim,type = 'n', 
       main = main, xlab = '',ylab='', adj = 0,
       axes = FALSE, xlim = xlim, ylim = ylim)
  
  if(opt$do.temp.fit){
    
    opt.param = as.list(opt$opt$par)
    
    weights = compute_weights(cycletable = cycletable, debug = FALSE)
    
    #temperature
    days.with.tmp = this.cycle$out_cycleday[this.cycle$temp>0]
    if(length(days.with.tmp)>0){
      tct = this.cycle[this.cycle$out_cycleday %in% days.with.tmp,]
      wc = weights[days.with.tmp]
      rt = range(tct$temp_c, na.rm = TRUE)
      if(rt[2] <= (rt[1]+0.2)){rt[1] = rt[1] - 0.05; rt[2] = rt[2] + 0.05}
      tct$adj.temp = adj.tmp.fn(tct$temp_c,rt,viz$margin.temp)
      #grid
      grid.value = seq(0.1*ceiling(10*rt[1]),0.1*floor(10*rt[2]),by = 0.1)
      adj.grid.value = adj.tmp.fn(grid.value, rt, viz$margin.temp)
      abline(h = adj.grid.value , col = 'white')
      
      # fitted curve
      time.vect = seq(1,max(cycletable$out_cycleday),by=0.1)
      temp.curve.optimized = temp_curve(param = opt.param, time.vect = time.vect)
      
      # plot the residues
      m = match(opt$opt$temp.obs, names(opt$opt$hmm.residues))
      l = opt$opt$hmm.residues[m]
      l = l[-length(l)]
      l[!is.finite(l)] = 1
      
      res = approx(x = 1:length(l), y = l, xout = time.vect, method = "linear")
      
      y = adj.tmp.fn(temp.curve.optimized$temp,rt,viz$margin.temp) 
      
      #cat('before polygon \n')
      polygon(c(time.vect, rev(time.vect)), 
              c(pmin(y+res$y,1), pmax(rev(y-res$y),0)),
              col = rgb(0,0,1,0.1), border = NA)
      
      #cat('after polygon \n')
      
      # plotting temperature + time indication
      it = which(diff(tct$out_cycleday) ==1)
      for(i in it){segments(tct$out_cycleday[i],tct$adj.temp[i],
                            tct$out_cycleday[i+1],tct$adj.temp[i+1],lwd = 0.5)}
      
      med.time = median(tct$time_temp)
      time.diff = tct$time_temp - med.time
      time.diff.index = pmin(pmax((time.diff*2)+7,0),15)
      colors.time = as.character(time.temp.gradient[time.diff.index])
      #cat("colors.time ",colors.time, "\n")
      
      points(tct$out_cycleday, tct$adj.temp, type = 'p', pch = 16, cex = wc*1.5, col = colors.time)
      text(tct$out_cycleday, tct$adj.temp, labels = as.character(tct$time_temp), pos = 3, cex = 0.5)
      
      # plot the curve
      points(time.vect, 
             adj.tmp.fn(temp.curve.optimized$temp,rt,viz$margin.temp), 
             col = 'blue', type = 'l')
    }
    
    # temperature optimization parameters
    if(!publi){
      seq.temp = opt$opt$temp.obs
      seq.temp = seq.temp[-length(seq.temp)]
      rect(xleft = c(1:xlim[2])-0.5, xright = c(1:xlim[2])+0.5,
           ybottom = -lh, ytop = 0,
           col = temp.hmm.colors[match(seq.temp,temp.hmm.symbols)],
           border = 'white'
      )
    }
  }
  #### HMM 
  # MBC
  
  state.seq.MBC = opt$hmm.results.MBC$obs.viterbi
  state.seq.MBC = state.seq.MBC[-length(state.seq.MBC)]
  if((!publi) | (publi & !opt$do.temp.fit)){
    if(publi){ybottom = -lh; ytop = 0}else{ybottom = -2*lh; ytop = -lh}
    rect(xleft = c(1:xlim[2])-0.5, xright = c(1:xlim[2])+0.5,
         ybottom = ybottom, ytop = ytop,
         col = cycle.states.dict$colors[match(state.seq.MBC,cycle.states.dict$states)],
         border = 'white'
    )
  }
  
  # all
  if(opt$do.temp.fit){
    state.seq.all = opt$hmm.results.all$obs.viterbi
    state.seq.all = state.seq.all[-length(state.seq.all)]
    if(publi){ybottom = -lh; ytop = 0}else{ybottom = -3*lh; ytop = -2*lh}
    rect(xleft = c(1:xlim[2])-0.5, xright = c(1:xlim[2])+0.5,
         ybottom = ybottom, ytop = ytop,
         col = cycle.states.dict$colors[match(state.seq.all,cycle.states.dict$states)],
         border = 'white'
    )
  }
  
  # AXES
  min.d = min(this.cycle$out_cycleday)
  max.d = max(this.cycle$out_cycleday)
  axis(1,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
  axis(1,at = this.cycle$out_cycleday, 
       tick = FALSE, padj = -1)
  axis(2, at = adj.grid.value, labels = grid.value, las = 1, lwd = 0, lwd.tick = 1, pos = min(this.cycle$out_cycleday)- 0.6)
  if(publi){
    axis(2, at = -lh+lh/2, labels = 'HMM',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  }else{
    axis(2, at = -lh+lh/2, labels = 't fit',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
    axis(2, at = -2*lh+lh/2, labels = 'HMM MBC',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
    axis(2, at = -3*lh+lh/2, labels = 'HMM Comb',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  }
  cat("before matplots \n")
  
  if(!publi){
    prob.MBC = t(opt$hmm.results.MBC$obs.posterior)
    prob.MBC = prob.MBC[-nrow(prob.MBC),-ncol(prob.MBC)]
    
    if(all(is.finite(range(prob.MBC)))){
      par(las = 1, mar = c(2, 2.5, 3.5, 0.5) + 0.1)
      matplot(prob.MBC, type = 'l', col = cycle.states.colors, lty = 1, lwd = 2, axes = FALSE, main = 'HMM - MBC', adj = 0)
      box();axis(2);axis(1, at = seq(1, xlim[2],by = 1))
    }
  }
  
  if(opt$do.temp.fit){
    prob.all = t(opt$hmm.results.all$obs.posterior)
    prob.all = prob.all[-nrow(prob.all),-ncol(prob.all)]
    
    if(all(is.finite(range(prob.all)))){
      if(!publi){par(las = 1, mar = c(2, 2.5, 3.5, 0.5) + 0.1)}
      if(publi){
        matplot(prob.all*scale.hmm-scale.hmm-n.lines*lh, type = 'l', col = cycle.states.colors, lty = 1, lwd = 2, axes = FALSE, add = TRUE)
        axis(2, at = seq(ylim[1],ylim[1]+scale.hmm,len = 5), labels = seq(0,1,len = 5),pos = min(this.cycle$out_cycleday)- 0.5)
      }else{
        matplot(prob.all, type = 'l', col = cycle.states.colors, lty = 1, lwd = 2, axes = FALSE, main = 'HMM - Comb', adj = 0)
        box();axis(2);axis(1, at = seq(1, xlim[2],by = 1))
      }
      
    }
  }
  
  
}





plot_fit_results_hmm = function(cycletable = cycletable, hmm.res = hmm.res) {
  
  this.cycle = cycletable
  
  # we first plot the cycle itself (just the observations)
  
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
         widths=c(1), heights=c(2,1))
  
  p = plot_cycle(subtable = cycletable, xaxis.bottom = FALSE, xaxis.top = TRUE, show_phases = FALSE, show_weights = TRUE, show_stars = FALSE)
  
  # we add the T low and T high
  if(p$rt[2]!=p$rt[1]){
  ovu = this.cycle$out_cycleday[which(res$obs.viterbi == 'O')]
  low.t.adj = adj.tmp.fn(res$T.low,p$rt,p$margin.temp)
  segments(x0 = min(this.cycle$out_cycleday),x1 = ovu-1, y0 = low.t.adj, y1 = low.t.adj, lwd = 3, col = rgb(0,0,0,0.3))
  high.t.adj = adj.tmp.fn(res$T.low+res$DT,p$rt,p$margin.temp)
  segments(x0 = ovu+1, x1 = max(this.cycle$out_cycleday) , y0 = high.t.adj, y1 = high.t.adj, lwd = 3, col = rgb(0,0,0,0.3))
  }
  
  # now we plot the HMM results
  
  scale.hmm = 0.6
  n.lines = 1.25
  lh = 0.2
  
  rc = range(cycletable$out_cycleday)
  xlim = c(0,rc[2])
  ylim = c(-n.lines*lh-scale.hmm,0)
  adj.grid.value = c(0,0.5,1)
  grid.value =  rep('',3) 
  
  main = paste('ovu: ',round(hmm.res$ovu, digits = 2),
               ' ± ', round(hmm.res$ovu.sd, digits = 2),
               ' | p(seq): ', round(hmm.res$prob.seq, digits = 3),
               ' | p(states): ', round(hmm.res$prob.seq.viterbi, digits = 3),
               ' | T° shift: ', round(hmm.res$DT, digits = 2))
  
  par(las = 1, mar = c(2, 2.5, 1, 0.5) + 0.1)
  plot(xlim,ylim,type = 'n', 
       main = '', xlab = '',ylab='', adj = 0,
       axes = FALSE, xlim = xlim, ylim = ylim)
  title(main,font.main = 1, adj = 0.2)
  

  # VITERBI SEQUENCE

  state.seq = res$obs.viterbi
  state.seq = state.seq[-length(state.seq)]
  ybottom = -lh; ytop = 0
  rect(xleft = this.cycle$out_cycleday-0.5, xright = this.cycle$out_cycleday+0.5,
        ybottom = ybottom, ytop = ytop,
        col = cycle.states.dict$colors[match(state.seq,cycle.states.dict$states)],
        border = 'white')
  
  
  # AXES
  min.d = min(this.cycle$out_cycleday)
  max.d = max(this.cycle$out_cycleday)
  axis(1,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
  axis(1,at = this.cycle$out_cycleday, 
       tick = FALSE, padj = -1)

  axis(2, at = -lh+lh/2, labels = 'HMM',tick = FALSE, pos = min(this.cycle$out_cycleday)- 0.1)
  
  
  # POSTERIOR PROB
  cat("before matplots \n")
  
  prob.all = t(res$obs.posterior)
  prob.all = prob.all[-nrow(prob.all),-ncol(prob.all)]
    
  if(all(is.finite(range(prob.all)))){
      matplot(this.cycle$out_cycleday, prob.all*scale.hmm-scale.hmm-n.lines*lh, type = 'l', col = cycle.states.colors, lty = 1, lwd = 2, axes = FALSE, add = TRUE)
      axis(2, at = seq(ylim[1],ylim[1]+scale.hmm,len = 5), labels = seq(0,1,len = 5),pos = min(this.cycle$out_cycleday)- 0.5)
  }
  par(mfrow = c(1,1))
}






################
# PLOT tracking history
################


plot.tracking.history = function(d = cycledays, 
                                 show_tests = FALSE, 
                                 show_consec_cycles = FALSE,
                                 show_fertile_window = FALSE,
                                 show_goal = TRUE, 
                                 relative_date = TRUE){
                                 	
                                 	
  #### IMPLEMENT SHOW_GOAL
  
  n.lines = 5 # number of tracking lines (besides temperature)
  if(show_tests){n.lines = 7}
  n.lines.temp = 3
  
  par(las = 1, mar = c(2, 4, 2, 0.5) + 0.1)
  

  d$user_input = (d$input_source>0)
  
  main = paste('user ID: ',unique(d$user_id))
  
  ylim = c(0, n.lines + n.lines.temp)
  
  plot(range(d$obs_day), ylim, type = 'n' , axes = FALSE, 
       xlab = '', ylab = '', main = main, adj = 0, yaxs = 'i', ylim = ylim)
  abline(v = d$obs_day[which(d$out_cycleday == 1)], col = 'gray90')
  abline(h = c(0:n.lines,ylim[2]), col = 'gray90')
  abline(h = seq(n.lines+0.5, ylim[2]-0.5, by = 0.5), col = 'gray95', lty = 2)
  if(show_fertile_window){
    xleft = aggregate(date ~ cycle_id, d[d$fertile_window,], min); xleft = xleft$date
    xright = aggregate(date ~ cycle_id, d[d$fertile_window,], max); xright = xright$date
    
    rect(xleft = xleft, xright = xright,
         ybottom = 0, ytop = ylim[2],
         col = rgb(0,0,1,0.1), border = NA)
   }
  
  # any tracking
  line.tracking = 0
  col = 'black'
  if(show_consec_cycles){col = as.numeric(as.factor(d$consec_cycle_id))}
  if(show_goal){col =  dict$goals$colors[match(d$goal, dict$goals$goals)]}
  points(d$obs_day, rep(line.tracking+0.5, nrow(d)), pch = 16, cex =  d$user_input/3, col = col)
  
  # temperature
  min.temp = min(d$temp_c, na.rm = TRUE)
  points(d$obs_day, rep(n.lines, nrow(d))+(d$temp_c-min.temp)*2,
         type = 'l', col = 'gray90')
  temp.col = dict$temp$colors[as.numeric(cut(d$temp_c, dict$temp$values))]
  points(d$obs_day, rep(n.lines, nrow(d))+(d$temp_c-min.temp)*2,
         cex = 0.5, pch = 16, col = temp.col)
  
  
  #lo = loess(d$temperature ~ as.numeric(d$date), degree = 2)
  
  #points(d$date[!is.na(d$temperature)], 5+lo$fitted-min(d$temperature, na.rm = TRUE), type = 'l', col = 'gray40')
  
  for(c in unique(d$cycle_nb)){
    j = which(d$cycle_nb == c)
    segments(x0 = min(d$obs_day[j], na.rm = TRUE), 
            x1= max(d$obs_day[j], na.rm = TRUE), 
            y0 = n.lines+(median(d$temp_c[j], na.rm = TRUE)-min.temp)*2, 
            y1 = n.lines+(median(d$temp_c[j], na.rm = TRUE)-min.temp)*2, col= 'gray40')
  }
  
  # bleeding
  line.bleeding = show_tests*2 + 4
  #points(d$obs_day, rep(line.bleeding, nrow(d))+d$blood/4, pch = 16, 
  #       cex =  d$blood/2, 
  #       col = dict$bleeding$colors[match(d$blood, dict$bleeding$index)] )
  
  segments(x0 = d$obs_day, x1 = d$obs_day, 
 			y0 = line.bleeding, y1 = line.bleeding + d$blood/4,
 			col= dict$bleeding$colors[match(d$blood, dict$bleeding$index)] )      
  # mucus
  line.mucus = show_tests*2 + 3
  #points(d$obs_day, rep(line.mucus, nrow(d))+d$elixir/5, pch = 16, type = 'l', col = 'gray80')

  points(d$obs_day, rep(line.mucus, nrow(d))+d$elixir/5, pch = 16, 
         cex =  dict$mucus$cex[match(d$elixir, dict$mucus$index)]/4,
         col = dict$mucus$colors[match(d$elixir, dict$mucus$index)]
  )
  

  
  # cervix
  line.cervix = show_tests*2 + 2
  points(d$obs_day, rep(line.cervix, nrow(d))+(d$feel%%10)/5, pch = 21, 
         cex =  (d$feel%%10)/3,
         col = dict$cervix$colors[match(d$feel, dict$cervix$index)]
  )
  
  # sex
  line.sex = show_tests*2 + 1
  tot.sex = 3+show_tests*2
  points(d$obs_day, rep(line.sex, nrow(d))+d$sex/tot.sex,
         cex =  0.5,
         pch = dict$sex$symbols[match(d$sex, dict$sex$index)],
         col = dict$sex$colors[match(d$sex, dict$sex$index)],
         bg = dict$sex$bg[match(d$sex, dict$sex$index)]
  )
  
  # Tests
  if(show_tests){
    line.OPK = 2
    points(d$date, rep(line.OPK, nrow(d))+d$opk/3,
           cex = 0.5, pch = opk.dict$pch[match(d$opk, opk.dict$index)], 
           col = opk.dict$colors[match(d$opk, opk.dict$index)])
    line.preg = 1
    points(d$date, rep(line.preg, nrow(d))+d$preg_test/3,
           cex = 0.5, pch = preg.test.dict$pch[match(d$preg_test, preg.test.dict$index)], 
           col = preg.test.dict$colors[match(d$preg_test, preg.test.dict$index)])
  }
  
  xaxis = seq(min(d$obs_day),max(d$obs_day),by = 'quarter')
  i.l = seq(1,length(xaxis),by = 4)
  xaxis_label = xaxis[i.l];

  if(relative_date){xaxis_label = paste0('Y',0:(length(i.l)-1)) }
  
  axis(1, at =xaxis, labels = NA )
  axis(1, at =xaxis[i.l], labels = xaxis_label )

  
  axis.at = c(c(1:n.lines)-0.5, n.lines + 1.5)
  axis.labels = c('any tracking','sex','cervix','mucus','bleeding','temp')
  if(show_tests){axis.labels = c(axis.labels[1],'Preg. test','OPK', axis.labels[2:6])}
  axis(2, at = axis.at, 
       labels = axis.labels,
       tick = FALSE, pos = xaxis[1])
}





