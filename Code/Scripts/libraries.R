# libraries

package_list = c('data.table',
              'hexbin', 
              'ggplot2', 
              'ggthemes',
              'ggridges',
              'scales',
              'lubridate',
              'dplyr',
              'HMM',
              'reshape')


pckgs = installed.packages()

for(package in package_list){
  cat(package,"\n")
  need.to.install = (!(package %in% pckgs[,1]))
  if(need.to.install){cat('installing package ',package,'\n');install.packages(package,dependencies = TRUE)}
  library(package = package, character.only = TRUE)
}

rm(package_list, pckgs,need.to.install,package)

