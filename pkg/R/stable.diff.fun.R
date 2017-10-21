stable.diff.fun<-function(res.log.mat, electrodes, crit.npoints, interval, startmsec, endmsec){
  
  if (any(is.na(res.log.mat))){
    stop("The function does not work properly with NAs", call.=F)
  }
  
  # l'input è una matrice logica SENZA NA
  
  # trasformo in numerico
  res.log.mat=apply(res.log.mat, 2, as.numeric)
  
  # cambio per creare un vettore collassando.
  res.mat.vec=apply(res.log.mat, 2, function(x){paste(x, collapse="")})
  
  # require(stringr)
  
  # DEFINE CRITERION
  # look for n consecutive points
  mycriterion=paste(rep(1, crit.npoints), collapse="")
  
  
  crit.mat=matrix(FALSE, nrow=dim(res.log.mat)[1], ncol=dim(res.log.mat)[2]) #nota che è la dimensione di ogni oggetto n timepoitns x m colonne
  
  # crit list is the list with results in mseconds.
  crit.list=list(NULL)
  length(crit.list)=dim(res.log.mat)[2]
  names(crit.list)=colnames(res.log.mat) # res.log.mat is a matrix, so I use colnames
  
  for (i in 1:length(res.mat.vec)){
    crit.res=str_locate_all(res.mat.vec[i], paste(mycriterion, "+", sep=""))[[1]] #!!!!! NOTA IL + serve per il regexp e trova anche altro. Nota anche l'[[1]] è per accedere all'oggetto che viene creato in una lista.
    
    # check se dimensioni risultati sono maggiori di 0
    if (dim(crit.res)[1]>0){
      for (k in 1:dim(crit.res)[1]){
        crit.mat[crit.res[k,1]:crit.res[k,2], i ]=TRUE
        crit.list[[i]][[k]]=pointstomsec(crit.res[k,], dim(res.log.mat)[1], startmsec=startmsec, endmsec=endmsec)
      }
    }
  }
  
  #fine parte da incorporare
  
  stable.diff.res=list(res.log.mat=crit.mat, crit.list=crit.list)
  return(stable.diff.res)
}



