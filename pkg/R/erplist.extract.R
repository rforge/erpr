erplist.extract<-function(bases, numbers, win.ini=NULL, win.end=NULL, point.ini=NULL, point.end=NULL, erplist=NULL, startmsec=NULL, endmsec=NULL)
{
  
  # this function extract values from an existing erplist according to win.ini, win.end.
  # the functions should be modified in order to be able to support (as input)
  # win.ini and win.end
  
  # preliminary checks
  if (is.null(erplist)){
    stop("an erplist object containing ERP data frames must be specified!", call.=F)
  }
  
  ### get startmsec from the first object
  erpdf = erplist[[1]]
  
  if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
    startmsec=attr(erpdf, "startmsec")
    endmsec=attr(erpdf, "endmsec")
  }
  
  
  # preliminary checks
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  
  #### object checks
  object.names=c(paste(bases, numbers, sep=""))
  if (any(!object.names%in%names(erplist))){
    missing.objects=object.names[!object.names%in%names(erplist)]
    missing.object.collist=paste(missing.objects, "\n", sep="")
    stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
  }
  
  
  
  outlist=list()
  #length(outlist)=length(numbers*length(bases))
  
  for (k in 1:length(bases)){
    base=bases[k]
    for (i in 1:length(numbers))
    {
      x.temp=erplist[[paste(base,numbers[i], sep="")]]
      Subject_name=comment(erplist[[paste(base,numbers[i], sep="")]])
      
      if (!is.null(win.ini) & !is.null(win.end)){
        x.temp=x.temp[round(msectopoints(win.ini,dim(x.temp)[1],startmsec, endmsec)):
                        round(msectopoints(win.end,dim(x.temp)[1],startmsec, endmsec)),]
        
        
      } else {
        x.temp=x.temp[ point.ini:point.end,] # in case the exact points are specified.
        
        # create the new startmmsec and endmsec
        win.ini = pointstomsec(point.ini, dim(x.temp)[1], startmsec, endmsec)
        win.end = pointstomsec(point.end, dim(x.temp)[1], startmsec, endmsec)
      }
      
      attr(x.temp, "startmsec") = win.ini
      attr(x.temp, "endmsec") = win.end
      
      x.temp=list(x.temp)
      names(x.temp)=paste(base,numbers[i], sep="")
      
      #
      outlist=c(outlist, x.temp)
    }
  }
  return(outlist)
}