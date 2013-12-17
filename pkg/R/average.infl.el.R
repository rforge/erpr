#############
# SCALP INFLUENCE
############
# 
# per funzionare questa funzione necessita del pacchetto rpanel (le altre funzioni necessarie sono contenute all'interno della funzione).

# questa funzione fa una carrellata di tutti gli elettrodi. Il problema è che si potrebbero non vedere alcuni elettrodi nel panel.
#
# INPUT

# -base = in testo le prime lettere di ciascun oggetto dei soggetti
# -numbers = il numero dei soggetti di cui fare average e influenza
# - altri parametri. Parametri (come nella funzione scalp).




average.infl.el=function(base, numbers, smo=0, layout=1, ylims=12, yrev=FALSE, startmsec=-200, endmsec=1200, lwd=1, lty=1, env=.Global.env){

require(rpanel)

dat.temp=eval(parse(file="", text=paste(base,numbers[1], sep="")))#prendo il primo oggetto per i nomi degli elettrodi
electrodes=names(dat.temp)


# la funzione contiene all'interno una funzione che crea il panel. Questa funzione a sua volta contiene la funzione scalp.infl.endo, che è quella che effettivamente fa il grafico appoggiandosi alla funzione scalp.endo. 


average.infl.panel=function(panel)
	{

		average.infl.endo=function(base, numbers, electrode, outline, smo=0.5 , col="black", startmsec=-200, endmsec=1000, yrev=FALSE, interval=c(startmsec, endmsec), step=200, verticals=NULL,horizontals=NULL, x.axis="default", ylim=c(-10,10), lwd=1, lty=1,out.col="red", env=env)
	{
		average.temp=eval(parse(file="", text=paste(base,numbers[1],"$",electrode, sep="")), envir=env)
		for (i in 2:length(numbers))
		{
			average.temp=average.temp+eval(parse(file="", text=paste(base,numbers[i],"$",electrode, sep="")), envir=env)		}
		average=average.temp/length(numbers)
		
		eeg(average,smo=smo, col=col, startmsec=startmsec, endmsec=endmsec, yrev=yrev, interval=interval,step=step, verticals=verticals, horizontals=horizontals, x.axis=x.axis, ylim=ylim, lwd=lwd, lty=lty,main=electrode)
		
		average.excl=(average.temp-eval(parse(file="", text=paste(base,outline,"$",electrode, sep="")), envir=env))/(length(numbers)-1)
		comp.add(average.excl, col=out.col, lwd=2, smo=smo)
	legend("topright", legend=c("Average all", paste("Average no subj", outline)), pch=15, col=c(col,out.col), pt.bg=c(1:6), cex=1.2)
		
		}							

		
		average.infl.endo(base=base, numbers=numbers, electrode=panel$electrode, outline=panel$outnumber, smo=smo,  yrev=yrev, startmsec=startmsec, endmsec=endmsec, env=env)
		
		panel
		}
		panel <- rp.control() #se volessi creare più pannelli allora dovrei aggiungere un'altro panel.
       rp.listbox(panel, outnumber, numbers, labels=as.character(numbers), action = average.infl.panel, initval=numbers[1])
	   rp.listbox(panel, electrode, electrodes, labels=electrodes, action = average.infl.panel, initval=electrodes[1])
	   rp.do(panel, average.infl.panel)

	   
	   		
   }