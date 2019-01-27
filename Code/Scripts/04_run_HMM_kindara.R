
cycles.file.list = list.files(cycles.folder.selected)
cycles.file.list = cycles.file.list[7:45]

cycles.folder.running.HMM = paste0(IO$kindara_data_cycles,"04 HMM/")

cat(as.character(now()),'\n')

tic()
for(cycles.file in cycles.file.list){
  
  cat(cycles.file, '\n')
  file.number = substr(cycles.file, 7,nchar(cycles.file))
  
  load(paste0(cycles.folder.selected,cycles.file))
  load(paste0(days.folder.selected,'day',file.number))
  
  
  cycles$hmm_done = FALSE
  
  cycle_ids = unique(cycles$cycle_id[cycles$selected_for_hmm])
  
  selected_cycle_ids = cycle_ids
  
  for (cycle_id in selected_cycle_ids) {
    #cat(cycle_id, '\n')
    j = which(days$cycle_id == cycle_id)
    cycletable = days[j, ]
    k = which(cycles$cycle_id == cycle_id)
    
    res = try(most_likely_day_of_ovulation_hmm(cycletable = cycletable,
                                               debug = FALSE, no.print = TRUE),
              silent = TRUE)
    
    if(typeof(res)!= 'character'){res$cycle_id = cycle_id}
    
    #filename = paste0(res_folder, cycle_id,'_',gsub(':','-',now()),'.Rdata')
    filename = paste0(IO$res_folder, cycle_id,'.Rdata')
    save(res, file = filename)
    cycles$hmm_done[k] = TRUE
  }
  save(cycles, file = paste0(cycles.folder.running.HMM,cycles.file))
}
toc()
