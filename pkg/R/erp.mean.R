erp.mean<-function(base, numbers, win.ini, win.end, erplist=NULL, startmsec=NULL, endmsec=NULL, others=NULL, format="long", name.dep="Dep", name.newvar="electrode", numbers.name="Subject")
	{
		
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
  
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
	
	#### object checks
	object.names=paste(base, numbers, sep="")
	if (any(!object.names%in%names(erplist))){
		missing.objects=object.names[!object.names%in%names(erplist)]
		missing.object.collist=paste(missing.objects, "\n", sep="")
		stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
	}

	
	datall=NULL		
		for (i in 1:length(numbers))
			{
			x.temp=erplist[[paste(base,numbers[i], sep="")]]
			Subject_name=comment(erplist[[paste(base,numbers[i], sep="")]])
			x.temp=apply(x.temp[round(msectopoints(win.ini,dim(x.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(x.temp)[1],startmsec, endmsec)),],2, mean)
			x.temp=data.frame(t(x.temp))
			x.temp[, numbers.name]=numbers[i]
			x.temp$Subject_name=Subject_name
			datall=rbind(datall, x.temp)
			}
		rownames(datall)=1:dim(datall)[1]
	
	
	if (format=="wide"){
		return(datall)
	}
	
	if(format=="long"){
		
		temp=rearrange(deps=1:(length(datall)-2), oth=c(numbers.name, "Subject_name"), dataset=datall, name.dep=name.dep, name.newvar=name.newvar)
		# notice: length(datall)-2 because the last two columns are Subject and Subject_name
	}
	
	if (!is.null(others))
		{
			for (i in 1:length(others))
			{
			temp[, names(others)[i]]=others[i]
			}
		}
	return(temp)
	}
