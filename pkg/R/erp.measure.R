erp.measure<-function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, FUN=mean, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", ...)
	{

	temp=erp.fun(base=base, numbers=numbers, win.ini=win.ini, win.end=win.end, env=env, startmsec=startmsec, endmsec=endmsec, FUN=FUN)
	
	if(format=="long"){
		
		temp=rearrange(deps=1:(length(temp)-2),oth=c("Subject", "Subject_name"),dataset=temp, name.dep=name.dep, name.newvar=name.newvar)
		
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
