average.latency <-
function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, peaktype="max")
	{	
		datall=NULL		
			
		#### creo la funzione msectopoints
		## dati 1) il tempo iniziale 2) il tempo finale e 3) la lunghezza globale in punti converte l'argomento "a" da msec a punti
		msectopoints=function(a, lengthsegment, startmsec, endmsec){
		x=((a-(startmsec))*(lengthsegment-1))/(endmsec-(startmsec))
		return(x+1)}
		
		pointstomsec=function(a, lengthsegment, startmsec, endmsec){
			x=(((endmsec+abs(startmsec))*a)/(lengthsegment-1))-abs(startmsec)
			return(x+1)
			}
		####
		for (i in 1:length(numbers))
		{
		average.temp=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
		rownames(average.temp)=1:dim(average.temp)[1]
		Subject_name=comment(eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env))
		average.temp.peak=apply(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),],2, eval(parse(file="", text=peaktype)))
		
		peak.pnts=NULL
		for (k in 1:length(average.temp)){
			peak.pnts.temp=as.numeric(rownames(average.temp[average.temp[,k]==average.temp.peak[k],]))
			peak.pnts[k]=peak.pnts.temp[peak.pnts.temp>=round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec))&peak.pnts.temp<=round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec))][1]
			}
			
		peak.msec=pointstomsec(peak.pnts, dim(average.temp)[1], startmsec, endmsec)
		average.temp.lat=data.frame(t(peak.msec))
		names(average.temp.lat)=names(average.temp)#ripristino i nomi degli elettrodi, persi nei pasaggi
		average.temp.lat$Subject=numbers[i]
		average.temp.lat$Subject_name=Subject_name
		datall=rbind(datall, average.temp.lat)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
