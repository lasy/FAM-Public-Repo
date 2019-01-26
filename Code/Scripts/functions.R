
combine_emissionProbs_matrices = function(states, symbols.1, symbols.2, emissionProbs.1, emissionProbs.2, sep = '_',debug = FALSE){
  
  symbols = as.vector(outer(symbols.1, symbols.2, paste, sep=sep))
  
  emissionProbs = c()
  for(si in 1:length(states)){
    emissionProbs.si = as.vector(emissionProbs.1[si,] %*% t(emissionProbs.2[si,]))
    emissionProbs = c(emissionProbs, emissionProbs.si)
  }
  
  emissionProbs
  
  emissionProbs = matrix(emissionProbs,
                         nrow = length(states), ncol = length(symbols), 
                         byrow = TRUE, dimnames = list(states, symbols))
  
  return(list(symbols = symbols, matrix = emissionProbs))
}

