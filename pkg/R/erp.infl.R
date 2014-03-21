erp.infl <-
function(base, numbers, electrode, smo=0, startmsec=-200, endmsec=1200, lwd=1, lty=1, envir=.GlobalEnv, ...){




# la funzione contiene all'interno una funzione che crea il panel. Questa funzione a sua volta contiene la funzione scalp.infl.endo, che è quella che effettivamente fa il grafico appoggiandosi alla funzione scalp.endo. 


erp.infl.panel=function(panel)
	{

		erp.infl.endo=function(base, numbers, electrode, outline, smo=0 , col="black", startmsec=-200, endmsec=1000, interval=c(startmsec, endmsec), step=200, verticals=NULL,horizontals=NULL, x.axis="default", ylim=c(-10,10), lwd=1, lty=1,out.col="red", envir)
	{
		average.temp=eval(parse(file="", text=paste(base,numbers[1],"$",electrode, sep="")), envir=envir)
		for (i in 2:length(numbers))
		{
			average.temp=average.temp+eval(parse(file="", text=paste(base,numbers[i],"$",electrode, sep="")), envir=envir)		
		}
		average=average.temp/length(numbers)
		
		erp(average,smo=smo, col=col, startmsec=startmsec, endmsec=endmsec, interval=interval,step=step, verticals=verticals, horizontals=horizontals, x.axis=x.axis, ylim=ylim, lwd=lwd, lty=lty,main=electrode, ...)
		
		average.excl=(average.temp-eval(parse(file="", text=paste(base,outline,"$",electrode, sep="")), envir=envir))/(length(numbers)-1)
		erp.add(average.excl, col=out.col, lwd=2, smo=smo)
	legend("topright", legend=c("Average all", paste("Average no subj", outline)), pch=15, col=c(col,out.col), pt.bg=c(1:6), cex=1.2)
		
		}							

		
		erp.infl.endo(base=base, numbers=numbers, electrode=electrode, outline=panel$outnumber, smo=smo, startmsec=startmsec, endmsec=endmsec, envir=envir)
		
		panel
		}
		panel <- rp.control() #se volessi creare più pannelli allora dovrei aggiungere un'altro panel.
       rp.listbox(panel, outnumber, numbers, labels=as.character(numbers), action = erp.infl.panel, initval=numbers[1])
	   rp.do(panel, erp.infl.panel)

	   
	   		
   }