cluster.sel<-function(results, cluster.res, cluster){
  # this function start from cluster results (of cluster based permutation)
  # and return a dataframe.
  el_names=names(results)
  
  sel_results=as.matrix(results)
  sel_results[cluster.res!=cluster]=NA
  
  sel_results=as.data.frame(sel_results)
  names(sel_results)=el_names
  
  return(sel_results)
}