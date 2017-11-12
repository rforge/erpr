grandaverage.bands = function (grandaverage = NULL, grandaverage.se = NULL, electrode = NULL){
  # function to create (for ggplot)
  # bands around the mean
  
  res = data.frame(lower = grandaverage[, electrode] - grandaverage.se[, electrode], upper = grandaverage[, electrode] + grandaverage.se[, electrode])
  
  # add electrode (with names)
  res[, electrode]= grandaverage[, electrode]
  
  return(res)
  
}