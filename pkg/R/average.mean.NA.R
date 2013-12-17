average.mean.NA <-
function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200)
	{	
		datall=NULL		
			
		#### creo la funzione msectopoints
		## dati 1) il tempo iniziale 2) il tempo finale e 3) la lunghezza globale in punti converte l'argomento "a" da msec a punti
		msectopoints=function(a,lengthsegment, startmsec, endmsec){
		x=((a-(startmsec))*lengthsegment)/(endmsec+abs(startmsec))
		return(x)}
		####
		for (i in 1:length(numbers))
		{
		average.temp=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
		Subject_name=comment(eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env))
		
		if (!all(average.temp[,1]==0)){
		average.temp=colMeans(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),])
		average.temp=data.frame(t(average.temp))} else {
		
		average.temp=data.frame(average.temp[1,])
		average.temp[1,]=NA
		}
		
		average.temp$Subject=numbers[i]
		average.temp$Subject_name=Subject_name
		datall=rbind(datall, average.temp)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
