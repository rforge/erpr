rearrange <-
  function(deps, oth=NULL, dataset, name.dep="Dep", name.newvar="New_Var") {
    dep=as.vector(as.matrix(dataset[,deps]))
    others=NULL
    if (length(oth)>0){
      for (i in 1:dim(dataset[,deps])[2]){
        others=rbind(others, as.data.frame(dataset[,oth]))
      }
      # add others, keeping consistency with previoous version
      # (you can specify either the indices or the names)
      if (is.numeric(oth)){
        names(others) = names(dataset)[oth]
      }
      if (is.character(oth)){
        names(others)=oth
      }
      
    }
    nuova.var=NULL
    for (i in 1:length(names(dataset[,deps]))) {
      nuova.var=c(nuova.var,rep(names(dataset[,deps])[i], dim(dataset[,deps])[1]))
    }
    dat=as.data.frame(cbind(dep,others))
    names(dat)[1]=paste(name.dep)
    dat[,paste(name.newvar)]=as.factor(nuova.var)
    return(dat)
  }
