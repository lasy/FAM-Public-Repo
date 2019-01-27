
################
# PLOT an individual cycle
################

plot_cycle = function(subtable = subtable, 
                      cycle_nb = NA, 
                      user = '0', 
                      xaxis.top = FALSE, 
                      xaxis.bottom = TRUE, 
                      show_weights = FALSE,
                      show_temp_time = TRUE, 
                      show_goal = FALSE){
  
  if(is.na(cycle_nb)){ cycle_nb = unique(subtable$cycle_number)[1]}
  if(user == '0'){ user = unique(subtable$user_id)[1]}
  
  k = which(subtable$user_id == user)
  subtable = subtable[k,]
  k = which(subtable$cycle_number == cycle_nb)
  this.cycle = subtable[k,]
  n.days = max(this.cycle$length)
  n.obs = nrow(this.cycle)
  
  cycle_id = unique(subtable$cycle_id)[1]
  
  goal = unique(this.cycle$goal)
  #cat(goal,'\n')
  #mode = unique(this.cycle$method)
  #cat(mode,'\n')

  n.lines = 5
  lw = 0.15
  par(las = 1, mar = c(1.5, 2.5, 4, 0.5) + 0.1)
  main = paste0('ID: ',user, ' | Cycle: ',cycle_nb)
  if(show_goal){main = paste0(main, ' | presumed goal: ',goal,"\n")}else{main = paste0(main,"\n")}
  xlim = range(c(0,n.days))
  ylim = c(-n.lines*lw-lw/2,1)
  plot(range(c(0.5,subtable$cycleday)),range(ylim),type = 'n', 
       main = main, xlab = '',ylab='', adj = 0,
       axes = FALSE, xlim = xlim, ylim = ylim+c(lw/2,0))
 
  #grid (vertical grid)
  for(d in 0:n.days){segments(x0 = d+0.5,y0 = -n.lines-1, x1 = d+0.5,y1 = 1, col = 'gray90')}
 
  # horizontal grid
  for(l in 2:n.lines){segments(x0 = -4, x1 = n.days+0.5, y0 = -l*lw+lw/2, y1 = -l*lw+lw/2, col = 'gray90')}
  
  
  #bleeding
  line.blood = -1 * lw
  points(this.cycle$cycleday, rep(line.blood,n.obs), 
         pch = 16, cex = this.cycle$bleeding, 
         col = dict$bleeding$colors[match(this.cycle$bleeding,dict$bleeding$index)])
  axis(2, at = line.blood, label = 'bleeding',tick = FALSE, pos = viz$pos.y.labels)
  
  
  #cervical mucus
  line.mucus = -2 * lw
  points(this.cycle$cycleday, rep(line.mucus,n.obs), 
         pch = dict$mucus$symbols[match(this.cycle$mucus,dict$mucus$index)], 
         cex = dict$mucus$cex[match(this.cycle$mucus,dict$mucus$index)], 
         col = dict$mucus$colors[match(this.cycle$mucus,dict$mucus$index)])
  axis(2, at = line.mucus, label = 'mucus',tick = FALSE, pos = viz$pos.y.labels)
  
  
  # cervix position
  line.cervix = -3 * lw
  x = this.cycle$cycleday;
  height = this.cycle$cervix_height
  height[(is.na(height))|(height == 0)] = 2;
  height = height - 2
  y = rep(line.cervix, n.obs) + height * lw/4
  openness = this.cycle$cervix_openness
  openness[(is.na(openness))|(openness == 0)] = 2;
  b = openness/3*lw/6
  firmness =  this.cycle$cervix_firmness
  border = dict$firmness$colors[match(firmness, dict$firmness$index)]
  
  j = ((this.cycle$cervix_height == 0)|is.na(this.cycle$cervix_height ))&
    ((this.cycle$cervix_openness == 0)|is.na(this.cycle$cervix_openness ))&
    ((this.cycle$cervix_firmness == 0)|is.na(this.cycle$cervix_firmness ))
  border[j] = NA 
  
  draw.ellipse(x = x, y = y, a = b*n.days*0.75, b = b, col = NA, border = border , lwd = 3)

  label.cervix = 'cervix'
  axis(2, at = line.cervix, label = label.cervix,tick = FALSE, pos = viz$pos.y.labels)
  
  #vaginal sensation
  line.feel = -4 * lw
  points(this.cycle$cycleday, rep(line.feel,n.obs), 
         pch = dict$feel$symbols[match(this.cycle$vaginal_sensation,dict$feel$index)], 
         cex = this.cycle$vaginal_sensation, 
         col = dict$feel$colors[match(this.cycle$vaginal_sensation,dict$feel$index)])
  axis(2, at = line.feel, label = 'vag. sens.',tick = FALSE, pos = viz$pos.y.labels)
  

  
  
  #sex
  line.sex = -5 * lw
  points(this.cycle$cycleday, rep(line.sex,n.obs), 
         pch = dict$sex$symbols[match(this.cycle$sex,dict$sex$index)], cex = 2, 
         col = dict$sex$colors[match(this.cycle$sex,dict$sex$index)], 
         bg = dict$sex$bg[match(this.cycle$sex,dict$sex$index)])
  axis(2, at = line.sex, label = 'sex',tick = FALSE, pos = viz$pos.y.labels)
  
  
  #temperature
  days.with.tmp = this.cycle$cycleday[(this.cycle$temperature>0)&!is.na(this.cycle$temperature)]
  if(length(days.with.tmp)>0){
    tct = this.cycle
    rt = range(tct$temperature, na.rm = TRUE)
    if(rt[2] <= (rt[1]+0.2)){rt[1] = rt[1] - 0.05; rt[2] = rt[2] + 0.05}
    
    tct$adj.temp = adj.tmp.fn(tct$temperature,rt,viz$margin.temp)
    
    grid.value = seq(0.1*ceiling(10*rt[1]),0.1*floor(10*rt[2]),by = 0.2)
    adj.grid.value = adj.tmp.fn(grid.value, rt, viz$margin.temp)
    n.grid = length(adj.grid.value)
    
    col.grid = 'gray90'
    segments(x0 = rep(0.5,n.grid), 
             x1 = rep(n.days+0.5,n.grid),
             y0 = adj.grid.value, y1 = adj.grid.value,
             col = col.grid)
    
    # plotting temperature + time indication
    iqt = tct$cycleday
    jiqt = which(tct$questionable_temp)
    if(length(jiqt)>0){iqt = iqt[-c(jiqt,jiqt-1)]}
    it = which(diff(tct$cycleday) ==1)
    it = intersect(it, iqt)
    for(i in it){segments(tct$cycleday[i],tct$adj.temp[i],
                          tct$cycleday[i+1],tct$adj.temp[i+1],lwd = 0.5)}
    
    
    if(show_weights){
      weights = compute_weights(cycletable = tct)
      weights[is.na(weights)] = 0.05
      col.t = paste0('gray',100-round(100*weights))
    }else{col.t = 'black'}
    
    points(tct$cycleday, tct$adj.temp, type = 'p', pch = 16, cex = 1, col = col.t)
    if(show_temp_time){
    text(tct$cycleday, tct$adj.temp, labels = substr(as.character(tct$temp_time),1,5), pos = 3, cex = 0.5)
    }
    
  }else{adj.grid.value = c(0.5); grid.value = c(NA); rt = c(0,0)}
  
  #peak day (jour sommet)
  if(any(this.cycle$peak_day, na.rm = TRUE)){
  points(max(this.cycle$cycleday[this.cycle$peak_day])+0.5, 0 ,
         pch = 25 , cex = 2, col = NA,
         bg = 'green3')
  }

  #axes
  min.d = 1
  max.d = n.days
  if(xaxis.bottom){
    axis(1,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
    axis(1,at = seq(1:n.days), tick = FALSE, padj = -1)
  }
  if(xaxis.top){
    axis(3,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
    axis(3,at = seq(1:n.days), tick = FALSE, padj = 1)
  }
  axis(2, at = adj.grid.value, labels = grid.value, las = 1, lwd = 0, lwd.tick = 1, pos = viz$pos.y.labels - 0.2)
  
  return(list(rt= rt, margin.temp = viz$margin.temp))
}


adj.tmp.fn = function(t,rt,margin.temp){
  if(rt[1]<rt[2]){
    (t - rt[1])/(rt[2]-rt[1])*(1-2*margin.temp)+margin.temp
  }else{0.5}
}






plot_fit_results_hmm = function(cycletable = cycletable, cycles = cycles, hmm.res = NA, show_temp_time = TRUE) {
  
  this.cycle = cycletable
  n.days = unique(this.cycle$length)[1]
  cycle_id = unique(this.cycle$cycle_id)[1]
  all.days = 1:n.days
  
  # we first plot the cycle itself (just the observations)
  
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
         widths=c(1), heights=c(2,1))
  
  p = plot_cycle(subtable = this.cycle, xaxis.bottom = FALSE, xaxis.top = TRUE,  show_weights = TRUE, show_temp_time = show_temp_time)
  
  
  if(!is.na(hmm.res)){
    T.low = res$T.low; DT = res$DT; 
    ovu.viterbi = res$ovu.viterbi
    ovu = res$ovu; ovu.sd = res$ovu.sd
    confidence = res$confidence
    posterior = t(res$obs.posterior)
    posterior = posterior[-nrow(posterior),-ncol(posterior)]
    viterbi.seq = res$obs.viterbi
  }else{
    j = which(cycles$cycle_id == cycle_id);
    T.low = cycles$low.T[j]; DT = cycles$D.T[j]; 
    ovu.viterbi = cycles$ovu.viterbi[j];
    ovu = cycles$ovu[j]; ovu.sd = cycles$ovu.sd[j]
    confidence = cycles$confidence[j]
    i.post = match(cycle.states[1:9], colnames(this.cycle))
    posterior = this.cycle[,i.post[!is.na(i.post)]]
    viterbi.seq = this.cycle$states
  
  }
  
  # we add the T low and T high
  if(p$rt[2]!=p$rt[1]){
    low.t.adj = adj.tmp.fn(T.low,p$rt,p$margin.temp)
    segments(x0 = 1,x1 = ovu.viterbi-1, y0 = low.t.adj, y1 = low.t.adj, lwd = 3, col = rgb(0,0,0,0.3))
    high.t.adj = adj.tmp.fn(T.low+DT,p$rt,p$margin.temp)
    segments(x0 = ovu.viterbi+1, x1 = max(unique(this.cycle$length)) , y0 = high.t.adj, y1 = high.t.adj, lwd = 3, col = rgb(0,0,0,0.3))
  }
  
  # now we plot the HMM results
  
  scale.hmm = 0.6
  n.lines = 1.25
  lh = 0.2
  
  rc = range(1,this.cycle$length)
  xlim = c(0,rc[2])
  ylim = c(-n.lines*lh-scale.hmm,0)
  adj.grid.value = c(0,0.5,1)
  grid.value =  rep('',3) 
  
  main = paste('ovu: ',round(ovu, digits = 2),
               ' ± ', round(ovu.sd, digits = 2),
              # ' | p(seq): ', round(hmm.res$prob.seq, digits = 3),
              # ' | p(states): ', round(hmm.res$prob.seq.viterbi, digits = 3),
              ' | confidence:', round(confidence, digits = 2),
               ' | T° shift: ', round(DT, digits = 2))
  
  par(las = 1, mar = c(2, 2.5, 1, 0.5) + 0.1)
  plot(xlim,ylim,type = 'n', 
       main = '', xlab = '',ylab='', adj = 0,
       axes = FALSE, xlim = xlim, ylim = ylim)
  title(main,font.main = 1, adj = 0.2)
  
  
  # VITERBI SEQUENCE
  
  state.seq = viterbi.seq
  state.seq = state.seq[1:n.days]
  ybottom = -lh; ytop = 0
  rect(xleft = all.days-0.5, xright = all.days+0.5,
       ybottom = ybottom, ytop = ytop,
       col = cycle.states.dict$colors[match(state.seq,cycle.states.dict$states)],
       border = 'white')
  
  
  # AXES
  min.d = 1
  max.d = n.days
  axis(1,at = seq(min.d-0.5,max.d+0.5,by = 1), labels = rep('',max.d-min.d+2))
  axis(1,at = 1:n.days, 
       tick = FALSE, padj = -1)
  
  axis(2, at = -lh+lh/2, labels = 'HMM',tick = FALSE, pos = viz$pos.y.labels)
  
  
  # POSTERIOR PROB
  cat("before matplots \n")
  
  prob.all = posterior

  if(all(is.finite(range(prob.all)))){
    matplot(all.days, prob.all*scale.hmm-scale.hmm-n.lines*lh, type = 'l', col = cycle.states.colors, lty = 1, lwd = 2, axes = FALSE, add = TRUE)
    axis(2, at = seq(ylim[1],ylim[1]+scale.hmm,len = 5), labels = seq(0,1,len = 5),pos =  0.5)
  }
  par(mfrow = c(1,1))
}


################
# PLOT tracking history
################


plot.tracking.history = function(d = days, 
                                 show_tests = FALSE, 
                                 show_consec_cycles = FALSE,
                                 show_fertile_window = FALSE, 
                                 relative_date = TRUE){
  
  n.lines = 5 # number of tracking lines (besides temperature)
  if(show_tests){n.lines = 7}
  n.lines.temp = 3
  
  par(las = 1, mar = c(2, 4, 2, 0.5) + 0.1)
  
  
  k = (d$bleeding > 0) |
    (!is.na(d$mucus)) |
    (!is.na(d$temperature)) |
    (d$opk > 0) |
    (d$sex > 0) |
    (d$preg_test > 0) |
    (d$vaginal_sensation > 0) |
    (d$cervix_firmness > 0) |
    (d$cervix_height > 0) |
    (d$cervix_openness > 0) 
  d$user_input = k
  
  main = paste('user ID: ',unique(d$user_id))
  
  ylim = c(0, n.lines + n.lines.temp)
  
  plot(range(d$date), ylim, type = 'n' , axes = FALSE, 
       xlab = '', ylab = '', main = main, adj = 0, yaxs = 'i', ylim = ylim)
  abline(v = d$date[which(d$first_day)], col = 'gray90')
  abline(h = c(0:n.lines,ylim[2]), col = 'gray90')
  abline(h = seq(n.lines+0.5, ylim[2]-0.5, by = 0.5), col = 'gray95', lty = 2)
  if(show_fertile_window){
    xleft = aggregate(date ~ cycle_id, d[d$fertile_window,], min); xleft = xleft$date
    xright = aggregate(date ~ cycle_id, d[d$fertile_window,], max); xright = xright$date
    
    rect(xleft = xleft, xright = xright,
         ybottom = 0, ytop = ylim[2],
         col = rgb(0,0,1,0.1), border = NA)}
  
  # any tracking
  line.tracking = 0
  col = 'black'
  if(show_consec_cycles){col = as.numeric(as.factor(d$consec_cycle_id))}
  points(d$date, rep(line.tracking+0.5, nrow(d)), pch = 16, cex =  d$user_input/3, col = col)
  
  # temperature
  min.temp = min(d$temperature, na.rm = TRUE)
  points(d$date, rep(n.lines, nrow(d))+(d$temperature-min.temp),
         type = 'l', col = 'gray90')
  temp.col = temp.dict$colors[as.numeric(cut(d$temperature, temp.dict$values))]
  points(d$date, rep(n.lines, nrow(d))+(d$temperature-min(d$temperature, na.rm = TRUE)),
         cex = 0.5, pch = 16, col = temp.col)
  
  
  #lo = loess(d$temperature ~ as.numeric(d$date), degree = 2)
  
  #points(d$date[!is.na(d$temperature)], 5+lo$fitted-min(d$temperature, na.rm = TRUE), type = 'l', col = 'gray40')
  
  for(c in unique(d$cycle_number)){
    j = which(d$cycle_number == c)
    segments(x0 = min(d$date[j], na.rm = TRUE), 
            x1= max(d$date[j], na.rm = TRUE), 
            y0 = n.lines+median(d$temperature[j], na.rm = TRUE)-min.temp, 
            y1 = n.lines+median(d$temperature[j], na.rm = TRUE)-min.temp, col= 'gray40')
  }
  
  # bleeding
  line.bleeding = show_tests*2 + 4
#  points(d$date, rep(line.bleeding, nrow(d))+d$bleeding/4, pch = 16, 
#         cex =  d$bleeding/2, 
#         col = dict$bleeding$colors[match(d$bleeding, dict$bleeding$index)] )
  
  segments(x0 = d$date, x1 = d$date, 
           y0 = line.bleeding, y1 = line.bleeding + d$bleeding/4,
           col= dict$bleeding$colors[match(d$bleeding, dict$bleeding$index)] )  
  
  # mucus
  line.mucus = show_tests*2 + 3
  points(d$date, rep(line.mucus, nrow(d))+d$mucus/20, pch = 16, 
         cex =  dict$mucus$cex[match(d$mucus, dict$mucus$index)]/3,
         col = dict$mucus$colors[match(d$mucus, dict$mucus$index)]
  )
  # cervix
  line.cervix = show_tests*2 + 2
  points(d$date, rep(line.cervix, nrow(d))+d$cervix_height/5, pch = 21, 
         cex =  d$cervix_openness/3,
         col = cervix.dict$colors[match(d$cervix_firmness, cervix.dict$index)]
  )
  
  # sex
  line.sex = show_tests*2 + 1
  tot.sex = 3+show_tests*2
  points(d$date, rep(line.sex, nrow(d))+d$sex/tot.sex,
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
  
  xaxis = seq(min(d$date),max(d$date),by = 'quarter')
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






#####################
# FIGURE 3
#####################



hist.by.cycleday = function(file = days.file, 
                            attr = 'mucus', 
                            breaks = 0:1,
                            groups.breaks = NA,
                            function.for.groups = NA,
                            from_start = FALSE,
                            cycleday_v = 1:28){
  load(file)
  cat('min',min(cycleday_v),'\n')
  cat('max',max(cycleday_v),'\n')
  
  
  if(attr == 'temp_diff_from_p25'){
    agg = aggregate(temperature ~ cycle_id, days, quantile, 0.25) ### TO DO THIS SHOULD MOVE IN 02 DATA PROCESSING
    m = match(days$cycle_id, agg$cycle_id) ### TO DO THIS SHOULD MOVE IN 02 DATA PROCESSING
    days$temp_diff_from_p25 = days$temperature - agg$temperature[m] ### TO DO THIS SHOULD MOVE IN 02 DATA PROCESSING
    y = days$temp_diff_from_p25
  }
  if(attr != 'temp_diff_from_p25'){ 
    eval(parse (text = paste0('y = days$',attr)))}
  
  
  
  if(from_start){ x = days$cycleday}else{x = days$cycleday_from_end}
  
  t.ok = (x >= min(cycleday_v)) &
    (x <= max(cycleday_v)) &
    (y > min(breaks)) & 
    (y < max(breaks)) &
    (!is.na(y))
  
  if(attr == 'temp_diff_from_p25'){t.ok = t.ok & (is.na(days$questionable_temp)| days$questionable_temp)  }
  
  cat(sum(t.ok, na.rm = TRUE),'\n')
  
  # here, make the groups
  # aggregate using the function for grouping

  if(!is.na(groups.breaks[1])){
    cmd = paste0('g = ',function.for.groups,'(days = days, groups.breaks = groups.breaks)')
    cat(cmd,'\n')
    eval(parse(text = cmd))
    }else{g = rep(1, length(y))}
  
  # loop over the groups
  unique.g = sort(unique(g))
  Hist.days = data.frame()
  for(ig in unique.g){
    # get the histograms
    s.ok = t.ok & (g == ig)
    hist.days = sapply(cycleday_v, hist.function, 
                       x = x[which(s.ok)], y = y[which(s.ok)], breaks = breaks )
    
    # add the group label to the row.names
    hist.days = as.data.frame(hist.days, 
                              row.names = paste0('group.',ig,'.',attr,'.breaks.',1:nrow(hist.days)),
                              col.names = paste0('d',cycleday_v))
    # collect results
    if(ig == unique.g[1]){Hist.days = hist.days}else{Hist.days = rbind(Hist.days, hist.days)}
  }
  
  return(Hist.days)
}


which.quant = function(X, pc = 0.50, n.cum, n.tot){
  which(n.cum[,X] >= (n.tot[X]*pc))[1]
}


hist.function = function(day = day,x = x, y = y, breaks = breaks){
  j = (x == day)
  y = y[j]
  h = hist(y, breaks = breaks, plot = FALSE)
  return(h$counts)
}

combine.sum = function(a,b){
  return(a+b)
}


rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}


#



#####################
# FIGURE 5
#####################


hist.cycles.par = function(file, 
                    attr = 'length', 
                    table.name = 'cycles',
                    breaks = 0:1, filter = TRUE){
  load(file)
  cat(file, '\n')

  eval(parse (text = paste0('y = ',table.name,'$',attr)))
  
  if((table.name == 'cycles') & filter){
    ok.cycles = cycles$reliable.ovu.est
  }else{ok.cycles = rep(TRUE, length(y))}
  
  cat(sum(ok.cycles, na.rm = TRUE),'\n')
  
  ok = ok.cycles & (y > min(breaks)) & 
    (y < max(breaks)) &
    (!is.na(y))
  
  cat(sum(ok, na.rm = TRUE),'\n')
  cat(round(sum(ok, na.rm = TRUE)/nrow(cycles),digits = 3),'\n')
  
  
  h = hist(y[ok], breaks = breaks, plot = FALSE)
  h = h$counts
  return(h)
}


group.by.bleeding.sum = function(days = days, groups.breaks = groups.breaks){
  j = (days$cycleday_from_end >= -10) 
  agg.bleeding.sum = aggregate(bleeding ~ cycle_id, days[j,], sum )
  
  bleeding.sum.groups = cut(agg.bleeding.sum$bleeding, groups.breaks)
  agg.bleeding.sum$group.num = as.numeric(bleeding.sum.groups)
  
  m = match(days$cycle_id, agg.bleeding.sum$cycle_id)
  g = agg.bleeding.sum$group.num[m]
  g[is.na(g)] = 1
  return(g)
}



#####################
# FIGURE 3C
#####################


compute.state.probs = function(file, 
                               fun = 'sum', #### NOT IMPLEMENTED
                               breaks = breaks,
                               cycleday_lim = c(-40, 40)){
  load(file)
  cat(file, '\n')
  
  
  cycle_length_bin = cut(days$length, breaks = breaks)
  days$cycle_length_bin = cycle_length_bin
  rm(cycle_length_bin)
  
  j = (days$cycleday_from_ovu >= cycleday_lim[1]) & (days$cycleday_from_ovu <= cycleday_lim[2])
  ci = which(colnames(days) %in% cycle.states[1:9])
  
  agg = aggregate( x = days[j,ci], 
                   by =  list(cycle_length_bin = days$cycle_length_bin[j], 
                              cycleday_from_ovu = days$cycleday_from_ovu[j]),FUN =  sum)
  
  
  agg_sum = aggregate(cycle_id ~ cycle_length_bin, days[j,], function(x) length(unique(x)))
  
  ci = which(colnames(agg) %in% cycle.states[1:9])
  colnames(agg)[ci] = paste0('state.',colnames(agg)[3:11])
  agg$cycle_length_bin_num = as.numeric(agg$cycle_length_bin)
  
  state_probs = reshape(agg, idvar =  c("cycle_length_bin","cycle_length_bin_num","cycleday_from_ovu"),
                        times = c("hM" , "lM", "lE" , "hE", "O", "Rise" , "hP" , "Ep" , "lP"), timevar = "state",
                        varying = list(3:11), v.names = "prob", direction = "long")
  
  state_probs$state = factor(state_probs$state, levels = cycle.states[1:9])
  
  j = which(state_probs$cycle_length_bin_num %in% range(state_probs$cycle_length_bin_num))
  state_probs = state_probs[-j,]
  
  state_probs$prob_sum = state_probs$prob
  m = match(state_probs$cycle_length_bin, agg_sum$cycle_length_bin)
  state_probs$n_cycles = agg_sum$cycle_id[m]
  state_probs$prob = state_probs$prob/state_probs$n_cycles
  
  return(state_probs)
}

combine.sum.state.probs = function(a,b){
  
  c = merge(a, b , by = c('cycle_length_bin','cycle_length_bin_num','cycleday_from_ovu','state'), all = TRUE)
  c$prob.x[is.na(c$prob.x)] = 0
  c$prob.y[is.na(c$prob.y)] = 0
  c$prob_sum.x[is.na(c$prob_sum.x)] = 0 
  c$prob_sum.y[is.na(c$prob_sum.y)] = 0 
  c$n_cycles.x[is.na(c$n_cycles.x)] = 0 
  c$n_cycles.y[is.na(c$n_cycles.y)] = 0 
  
  c$n_cycles = c$n_cycles.x + c$n_cycles.y
  c$prob_sum = c$prob_sum.x + c$prob_sum.y
  c$prob = c$prob_sum / c$n_cycles
  

  xy = which(colnames(c) %in% c('prob.x','prob.y','prob_sum.x','prob_sum.y','n_cycles.x','n_cycles.y'))
  c = c[,-xy]
  
  return(c)
}





