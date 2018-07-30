butterfly <-
  function(base, numbers,  electrode = NULL, startmsec=NULL, endmsec=NULL, erplist = NULL, outline=NULL, out.col="black", add = FALSE, ...)
    # ... further parameters are passed to erp
  {
    # preliminary checks
    if (is.null(erplist)){
      stop("an erplist object containing ERP data frames must be specified!", call.=F)
    }
    
    #electrode checks
    if (!electrode%in%names(erplist[[1]])) {
      stop("The electrode specified is not in the data frames contained in the erplist", call.=F)
    }
    
    #### object checks
    object.names=c(paste(base, numbers, sep=""))
    if (any(!object.names%in%names(erplist))){
      missing.objects=object.names[!object.names%in%names(erplist)]
      missing.object.collist=paste(missing.objects, "\n", sep="")
      stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
    }
    
    erpdf = erplist[[paste(base, numbers[1], sep="")]]
    # get startmsec from the first object
    if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
      startmsec=attr(erpdf, "startmsec")
      endmsec=attr(erpdf, "endmsec")
    }
    
    if (is.null(startmsec)|is.null(endmsec)){
      stop("startmsec and endmsec must be specified", call.=F)
    }
    
    # TO BE ADDED	
    #	if (is.null(interval)){
    #	  interval=c(startmsec, endmsec)
    #	}
    
    mycall=match.call()
    mycall.list=as.list(mycall)
    mycall.erp.add=mycall.list[names(mycall.list)%in%c("electrode", "lty", "smo", "col", "lwd", "startmsec", "endmsec", "interval")]
    mycall.erp.add=append(mycall.erp.add, as.name("erpdf"))
    names(mycall.erp.add)[length(mycall.erp.add)]="erpdf"
    # in the line above I retrieve the arguments of the call relavant for erp.add
    
    
    
    #numbers=numbers[-!(numbers%in%outline)]
    if (add==FALSE)
    {
      i=1
      erpdf=erplist[[paste(base, numbers[i], sep="")]]
      erp(erpdf, electrode, startmsec=startmsec, endmsec=endmsec,  ...)
      for (i1 in 2:length(numbers))
      {
        erpdf=erplist[[paste(base, numbers[i1], sep="")]]
        
        do.call("erp.add", mycall.erp.add)
      }
    }
    if (add==TRUE) 
    {
      for (i in 1:length(numbers))
      {
        erpdf=erplist[[paste(base, numbers[i], sep="")]]
        do.call("erp.add", mycall.erp.add)
      }	
    }
    if (!is.null(outline)){
      
      # modifications to call for outline electrodes
      
      #modification to lwd
      mycall.erp.add.out=mycall.erp.add
      if (!is.null(mycall.erp.add.out$lwd)){
        mycall.erp.add.out$lwd= mycall.erp.add.out$lwd+2
      } else {
        mycall.erp.add.out$lwd=3
      }
      # modification to col
      mycall.erp.add.out$col = out.col
      erpdf=erplist[[paste(base, numbers[outline], sep="")]] #select the subject to be outlined
      
      
      for (k in 1:length(outline))
      {
        do.call("erp.add", mycall.erp.add.out)
      }
    }	
  }
