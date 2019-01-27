

cycles.folder.HMM = paste0(IO$kindara_data_cycles,"05 post HMM")
days.folder.HMM = paste0(IO$kindara_data_days,"05 post HMM")


cycles.file.list = list.files(cycles.folder.running.HMM)
#cycles.file.list = cycles.file.list[26:45]

cat(as.character(now()),'\n')

tic()
for(cycles.file in cycles.file.list){
  
  cat(cycles.file, '\t\t',as.character(now()),'\n')
  file.number = substr(cycles.file, 7,nchar(cycles.file))
  
  load(paste0(cycles.folder.running.HMM,cycles.file))
  load(paste0(days.folder.selected,'day',file.number))
  
  cycle_ids = unique(cycles$cycle_id[cycles$hmm_done])
  cat('# of cycles:', length(cycle_ids),'\n')
  
  selected_cycle_ids = cycle_ids
  
  cycles.hmm = data.frame()
  days.hmm = data.frame()
  
  for (cycle_id in selected_cycle_ids) {

    # load the hmm result
    
    filename = paste0(IO$res_folder, cycle_id,'.Rdata')
    rm(res)
    load(file = filename)
    
    # expand the results into a vector for cycles and into a matrix for days
    
    this.cycles.hmm = c()
    this.days.hmm = data.frame()
    if (typeof(res) != "character") {

      this.cycles.hmm = data.frame(
        cycle_id = cycle_id,
        ovu = res$ovu,
        ovu.sd = res$ovu.sd,
        ovu.viterbi = res$ovu.viterbi,
        prob.seq = res$prob.seq,
        prob.states = res$prob.seq.viterbi,
        low.T = res$T.low,
        D.T = res$DT,
        confidence = res$confidence,
        stringsAsFactors = FALSE)
      
      posterior = t(res$obs.posterior)
      posterior = posterior[-nrow(posterior), -ncol(posterior)]
      states = res$obs.viterbi
      states = states[-length(states)]
      weights = res$weights
      
      
      this.days.hmm = data.frame(
        cycle_id = rep(cycle_id,nrow(posterior)),
        cycleday = as.numeric(rownames(posterior)),
        states = states,
        weights = weights,
        posterior,
        stringsAsFactors = FALSE)
      
      # save it to big matrices cycles.hmm and days.hmm
      if (nrow(cycles.hmm) == 0) {
        days.hmm = this.days.hmm
        cycles.hmm = this.cycles.hmm
      } else{
        days.hmm = rbind(days.hmm, this.days.hmm)
        cycles.hmm = rbind(cycles.hmm, this.cycles.hmm)
      }
    
    
    }
    
    
  }
  
  # match the cycles.hmm with cycles & add the columns from cycles.hmm to cycles
  c = merge(cycles, cycles.hmm, all = TRUE, by = 'cycle_id')
  o = order(c$user_id,c$cycle_number)
  cycles = c[o,]
  rm(c, o)
  
  # merge the days.hmm with the days table 
  # !!! days.hmm will be larger than days because it has data for every single day
  
  days$original_row  = TRUE
  
  d = merge(days, days.hmm, all = TRUE, by = c('cycle_id','cycleday'))

  m = match(d$cycle_id, cycles$cycle_id)
  
  d$user_id = cycles$user_id[m]
  d$start = cycles$start[m]
  d$end = cycles$end[m]
  d$cycle_number = cycles$cycle_number[m] 
  d$length = cycles$length[m]
  d$goal_txt = cycles$goal_txt[m]
  d$selected_for_hmm = cycles$selected_for_hmm[m]
  
  d$cycleday_from_end = d$cycleday - d$length  - 1
  d$date = d$start + days(d$cycleday - 1)
  
  d$original_row[is.na(d$original_row)] = FALSE
  
  o = order(d$user_id, d$cycle_number, d$cycleday)
  days = d[o,]
  rm(d, o, m)
  
  # save the new cycles and days
  
  save(cycles, file = paste0(cycles.folder.HMM,  cycles.file))
  save(days, file = paste0(days.folder.HMM,'day',file.number))
  
}
toc()



