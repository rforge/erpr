erp.fun.loop<-function(bases=NULL, numbers=NULL, erp.fun=NULL, intervals=NULL, conditions.names=NULL, erplist=NULL, startmsec=NULL, endmsec=NULL, others=NULL, intervals.var=TRUE,  ...){
  
  #datall=erp.fun.loop(bases=c("Exp1_word_subj", "Exp1_nonword_subj"), numbers=1:20, erp.fun=erp.mean, conditions.names=c("word", "nonword"), erplist=ERPsets, intervals.var=TRUE, intervals=list(c(100, 200), c(300, 500)))
  
  #initial checks
  
  if (is.null(bases)){
    stop("you must specify some \"bases\", as a vector of characters", call.=F)
  }
  
  if (is.null(numbers)){
    stop("you must specify some \"numbers\", as a vector of numbers", call.=F)
  }
  
  if (is.null(erp.fun)){
    stop("you must specify the erp.fun (e.g. erp.mean), to be applied
         Specify this function as a function, not as a text", call.=F)
  }
  
  if (is.character(erp.fun)){
    stop("the function to be applied must NOT specified as text.
         e.g. erp.mean and NOT \"erp.mean\".", call.=F)
  }
  
  if (is.null(intervals)){
    stop("you must specify the time intervals to be considered. 
         Time intervals are specified as a list of pairs of numbers 
         (e.g. list( c(200, 400) , c(400, 600))", call.=F)
  }
  
  if (length(conditions.names)!=length(bases)){
    stop("The length of condition names should be the same of bases.
         (one name for each base).", call.=F)
  }
  
  if (length(conditions.names)!=length(bases)){
    stop("The length of condition names should be the same of bases.
         (one name for each base).", call.=F)
  }
  
  # get startmsec from the first object
  erpdf=erplist[[1]]
  if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
    startmsec=attr(erpdf, "startmsec")
    endmsec=attr(erpdf, "endmsec")
  }
  
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  
  
  # create empty object
  dat=NULL
  
  
  for (a in 1:length(bases)){
    
    for (i in 1:length(intervals)){
    
      curr.base=bases[a]
      
      win.ini=intervals[[i]][1]
      win.end=intervals[[i]][2]
      

      others=NULL
      
      if (!is.null(conditions.names)){
        condition.name=conditions.names[a]
        others=c(others, condition=condition.name)
      }
      
      if (intervals.var){
        interval.name=paste(win.ini, win.end, sep="-")
        others=c(others, interval=interval.name)
      }
      
      temp.dat=erp.fun(base = curr.base, numbers=numbers, win.ini=win.ini, win.end=win.end, erplist=erplist, startmsec=startmsec, endmsec=endmsec, others=others, ...)

      dat=rbind(dat, temp.dat)
      
    }
  
    
  }
  
  return(dat)
}