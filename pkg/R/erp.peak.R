erp.peak<-function(base, numbers, win.ini, win.end, envir=.GlobalEnv, startmsec=-200, endmsec=1200, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", peak.fun=max, ...)
	{
	datall=NULL		
		for (i in 1:length(numbers))
			{
			x.temp=eval(parse(file="", text=paste(base,numbers[i], sep="")),envir=envir)
			Subject_name=comment(eval(parse(file="", text=paste(base,numbers[i], sep="")),envir=envir))
			x.temp=apply(x.temp[round(msectopoints(win.ini,dim(x.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(x.temp)[1],startmsec, endmsec)),],2, FUN=function(x){peak.fun(x, ...)})
			x.temp=data.frame(t(x.temp))
			x.temp$Subject=numbers[i]
			x.temp$Subject_name=Subject_name
			datall=rbind(datall, x.temp)
			}
		rownames(datall)=1:dim(datall)[1]
	
	
	if (format=="wide"){
		return(datall)
	}
	
	if(format=="long"){
		
		temp=rearrange(deps=1:(length(datall)-2),oth=c("Subject", "Subject_name"),dataset=datall, name.dep=name.dep, name.newvar=name.newvar)
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
