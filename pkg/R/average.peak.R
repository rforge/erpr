average.peak <-
function(base, numbers, win.ini, wi.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, peaktype="max")
	{	
		datall=NULL		
			
		#### creo la funzione msectopoints
		## dati 1) il tempo iniziale 2) il tempo finale e 3) la lunghezza globale in punti converte l'argomento "a" da msec a punti
	msectopoints=function(a, lengthsegment, startmsec, endmsec){
	x=((a-(startmsec))*(endmsec-1))/(endmsec-(startmsec))
	return(x+1)}
		####
		for (i in 1:length(numbers))
		{
		average.temp=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
		Subject_name=comment(eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env))
		average.temp=apply(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(wi.end,dim(average.temp)[1],startmsec, endmsec)),],2, eval(parse(file="", text=peaktype)))
		average.temp=data.frame(t(average.temp))
		average.temp$Subject=numbers[i]
		average.temp$Subject_name=Subject_name
		datall=rbind(datall, average.temp)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
