erplist.resample<-function(base=NULL, numbers=NULL, erplist=NULL, outname=paste(base, "_res", sep=""), sr.factor=c(1,2)){
  ####
  # function to resample ERP with resample function from (signal package)
  requireNamespace("signal")
  
   	# preliminary checks
	if (is.null(erplist)){
	stop("an erplist object containing ERP data frames must be specified!", call.=F)
	}
	
	#### object checks
  object.names = c(paste(base, numbers, sep = ""))
    if (any(!object.names %in% names(erplist))) {
        missing.objects = object.names[!object.names %in% names(erplist)]
        missing.object.collist = paste(missing.objects, "\n", 
            sep = "")
        stop("The following objects are not contained in the erplist specified:\n", 
            missing.object.collist, call. = F)
    }
	
	#if (!is.numeric(numbers)){
	#stop("\"numbers\" must be a numeric vector", call.=F)
	#}
  
	if (is.null(erplist)){
	stop("an erplist object containing ERP data frames must be specified", call.=F)
	}
	if (is.null(outname))
	{
	stop("the argument \"outname\" must be specified", call.=F)
	}

	outlist=list()
	length(outlist)=length(numbers)
	
	for (i in 1:length(numbers))
	{
		temp=erplist[[paste(base,numbers[i], sep="")]]
		temp.out=apply(temp, 2, function(x){signal::resample(x, p=sr.factor[1], q=sr.factor[2])})
		temp.out=as.data.frame(temp.out)
		# combine old comment with a new comment.
		comment(temp.out)=paste(comment(erplist[[paste(base,numbers[i], sep="")]]), 
			"  - resampled by a factor of:", sr.factor[1], "/", sr.factor[2], sep="")
	outlist[[i]]=temp.out
	names(outlist)[[i]]=paste(outname,numbers[i], sep="")
	}
	return(outlist)	
}