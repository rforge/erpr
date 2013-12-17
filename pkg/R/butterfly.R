butterfly <-
function(base, numbers,electrode, add=FALSE, smo=0.5 , col="black", startmsec=-200, endmsec=1000, yrev=FALSE, interval=c(startmsec, endmsec), step=200, verticals=NULL,horizontals=NULL, x.axis="default", ylim="default", lwd=1, lty=1, outline=NULL, out.col="black", envir=.GlobalEnv)
	{
		
env=deparse(substitute(envir))

			numbers=numbers[-!(numbers%in%outline)]
		if (add==FALSE)
			{
			i=1
			eeg(eval(parse(file="", text=paste(env,"$",base,numbers[i],"$",electrode, sep=""))), smo=smo, col=col, startmsec=startmsec, endmsec=endmsec, yrev=yrev, interval=interval, step=step, verticals=verticals, horizontals=horizontals, x.axis=x.axis, ylim=ylim, lwd=lwd, lty=lty)
			for (i in 2:length(numbers))
				{
				comp.add(eval(parse(file="", text=paste(env, "$", base,numbers[i],"$",electrode, sep=""))), col=col,lwd=lwd, lty=lty, smo=smo)
				}
			}
		if (add==TRUE) 
			{
			for (i in 1:length(numbers))
				{
				comp.add(eval(parse(file="", text=paste(env, "$", base,numbers[i],"$",electrode, sep=""))),col=col, lwd=lwd, lty=lty, smo=smo)
				}	
			}
		if (!is.null(outline)){
			for (k in 1:length(outline))
			{
			comp.add(eval(parse(file="", text=paste(env, "$",base,outline[k],"$",electrode, sep=""))),col=out.col, lty=lty, smo=smo, lwd=lwd+2)
			}
		}	
	}
