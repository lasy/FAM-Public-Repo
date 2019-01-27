

cycle.folder = "../../../../Apps/Kindara/Analysis/Rdata/Cycles/"
cycle.folder.hmm = paste0(cycle.folder,"04 HMM/")

file.list = paste0(cycle.folder.hmm,list.files(cycle.folder.hmm))

CYCLES = data.frame()
for(file in file.list){
  cat(file, "\n")
  load(file, verbose = TRUE)
  cat(dim(cycles), "\n")
  CYCLES = rbind(CYCLES, cycles)
}

save(CYCLES, file = paste0(cycle.folder, "04 hmm CYCLES.Rdata") )

###################


cycle.folder = "../../../../Apps/Kindara/Analysis/Rdata/Cycles/"
cycle.folder.hmm = paste0(cycle.folder,"05 post HMM/")

file.list = paste0(cycle.folder.hmm,list.files(cycle.folder.hmm))

CYCLES = data.frame()
for(file in file.list){
  cat(file, "\n")
  load(file, verbose = TRUE)
  cat(dim(cycles), "\n")
  CYCLES = rbind(CYCLES, cycles)
}

save(CYCLES, file = paste0(cycle.folder, "05 post hmm CYCLES.Rdata") )


