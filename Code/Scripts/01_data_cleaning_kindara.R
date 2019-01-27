
# clean account tables

cat('start cleaning accounts tables\n')
#clean account tables
folder.path = paste0(IO$kindara_data_accounts, "00 original/")
files = list.files(folder.path)
for(file in files){
  cat('\t',file, '\n')
  file.noext = unlist(strsplit(file, '\\.'))[1]
  filename = paste0(folder.path, file)
  accounts = read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  
  clean_accounts = clean_account_table(accounts, accounts.masterlist)
  accounts = clean_accounts
  save(accounts, file = paste0(IO$kindara_data_accounts, "01 cleaned/",file.noext,'.Rdata'))
  rm(accounts, clean_accounts)
  cat('\t >> cleaned\n')
}
cat('done cleaning accounts tables\n')



# create one file with all clean accounts
cat('start concatening accounts tables\n')
#clean account tables
folder.path = paste0(IO$kindara_data_accounts, "01 cleaned/")
files = list.files(folder.path)
for(file in files){
  cat('\t',file, '\n')
  filename = paste0(folder.path, file)
  load(filename)
  if(file == files[1]){accounts.onefile = accounts}else{accounts.onefile = rbind(accounts.onefile, accounts)}
  rm(accounts)
  cat('\t >> added\n')
}
accounts = accounts.onefile
save(accounts, file = paste0(IO$kindara_data_accounts, "cleaned_accounts_onefile.Rdata"))

rm(accounts, accounts.onefile)

cat('done concatening accounts tables\n')



# clean cycles tables

cat('start cleaning cycle tables\n')
folder.path = paste0(IO$kindara_data_cycles, "00 original/")
files = list.files(folder.path)
for(file in files){
  cat('\t',file, '\n')
  file.noext = unlist(strsplit(file, '\\.'))[1]
  filename = paste0(folder.path, file)
  cycles = read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  
  clean_cycles = clean_cycle_table(cycles, accounts.masterlist)
  cycles = clean_cycles
  save(cycles, file = paste0(IO$kindara_data_cycles, "01 cleaned/",file.noext,'.Rdata'))
  rm(cycles, clean_cycles)
  cat('\t >> cleaned\n')
}
cat('done cleaning cycles tables\n')



# clean days tables

cat('start cleaning days tables\n')
folder.path = paste0(IO$kindara_data_days, "00 original/")
files = list.files(folder.path)
for(file in files){
  cat('\t',file, '\n')
  file.noext = unlist(strsplit(file, '\\.'))[1]
  filename = paste0(folder.path, file)
  days = read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  days.o = days
  clean_days = clean_day_table(days, accounts.masterlist)
  days = clean_days
  save(days, file = paste0(IO$kindara_data_days, "01 cleaned/",file.noext,'.Rdata'))
  rm(days, clean_days, days.o)
  cat('\t >> cleaned\n')
}
cat('done cleaning days tables\n')

