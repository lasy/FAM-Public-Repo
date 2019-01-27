
## assemble individual fit results into a single table
# done in batches

N = length(selected_cycle_ids)
bs = 500
nb = ceiling(N/bs)
now = gsub(':','-',now())
bstart = 1
for(b in bstart:nb){
  cat('b:',b,'\n')
  results.by.cycle = c()
  results.cycleday = c()
  i.start = (b-1)*bs+1 
  i.stop = min(N,b*bs)
  for (cycle_id in selected_cycle_ids[i.start:i.stop]) {
    #cat(cycle_id, '\n')
    rm(res)
    filename = get_latest_filename(cycle_id = cycle_id, folder_path = IO$res_folder)
    load(filename)
    
    results.this.cycle = c()
    results.cycleday.this.cycle = data.frame()
    if (typeof(res) != "character") {
      #save the results in a table
      
      results.this.cycle = data.frame(
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
      
      results.cycleday.this.cycle = data.frame(
        cycle_id = rep(cycle_id,nrow(posterior)),
        out_cycleday = rownames(posterior),
        states = states,
        weights = weights,
        posterior)
      
      
      #invisible(readline(prompt="Press [enter] to continue"))
      if (cycle_id == selected_cycle_ids[i.start]) {
        results.cycleday = results.cycleday.this.cycle
        results.by.cycle = results.this.cycle
      } else{
        results.cycleday = rbind(results.cycleday, results.cycleday.this.cycle)
        results.by.cycle = rbind(results.by.cycle, results.this.cycle)
      }
    }
  }
  save(results.cycleday,file = paste0(IO$global_path_Rdata,'batch/results_cycleday_',now,'_',b,'.Rdata'))
  save(results.by.cycle,file = paste0(IO$global_path_Rdata,'batch/results_by_cycle_',now,'_',b,'.Rdata'))
}


# put the batches together

results.by.cycle.all = c()
results.cycleday.all = c()
for(b in 1:nb){
  load(paste0(IO$global_path_Rdata,'batch/results_cycleday_',now,'_',b,'.Rdata'))
  load(paste0(IO$global_path_Rdata,'batch/results_by_cycle_',now,'_',b,'.Rdata'))
  if(b == 1){
    results.by.cycle.all = results.by.cycle
    results.cycleday.all = results.cycleday
  }else{
    results.by.cycle.all = rbind(results.by.cycle.all,results.by.cycle)
    results.cycleday.all = rbind(results.cycleday.all, results.cycleday)
  }
}
save(results.by.cycle.all,file = paste0(IO$global_path_Rdata,'fit_results_by_cycle_all_',now,'.Rdata'))
save(results.cycleday.all,file = paste0(IO$global_path_Rdata,'fit_results_cycleday_all_',now,'.Rdata'))

save(results.by.cycle.all,file = paste0(IO$restricted_data_folder,'fit_results_by_cycle_all_',now,'.Rdata'))
save(results.cycleday.all,file = paste0(IO$restricted_data_folder,'fit_results_cycleday_all_',now,'.Rdata'))


# bind cycle and cycledays tables with fit results

m = match(cycles$cycle_id, results.by.cycle.all$cycle_id)
cycles = cbind(cycles, results.by.cycle.all[m,-1])


results.cycleday.all$day_id = paste0(results.cycleday.all$cycle_id,'_',results.cycleday.all$out_cycleday )
m = match(cycledays$day_id, results.cycleday.all$day_id)
k = which(colnames(results.cycleday.all) %in% c('cycle_id','out_cycleday','day_id'))
cycledays = cbind(cycledays, results.cycleday.all[m, - k ] )



