	msectopoints<-function(a, lengthsegment, startmsec, endmsec){
	
	totmsec=endmsec-(startmsec) #total duration in msec
	msecstep=lengthsegment/totmsec #how many points is a msec.
	x=(a-startmsec)*msecstep
	return(x)
	
	}
