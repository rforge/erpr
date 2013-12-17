latency.analysis <-
function(base1, base2=NULL, numbers1, numbers2=numbers1, Electrodes, win.ini, win.end, paired=T, env=.GlobalEnv, baseline=-200, total.length=1200, crit.msec=50)
	{	
	
	dat.list=NULL		
			
	# questa è la nuova funzione msectopoint.values (creata per eeg2). E' più corretta della precedente
	msectopoint.values=function(a, lengthsegment, startmsec, endmsec){
	x=((a-(startmsec))*(lengthsegment-1))/(endmsec-(startmsec))
	return(x+1)}
	
	# recupero il numero di timepoint.values dal primo file 
	temp1=eval(parse(file="", text=paste(base1,numbers1[1], sep="")),env=env)
	tot.timepoint.values=dim(temp1)[1]
	samp.rate=sampling.rate(temp1, baseline, total.length)
	## converto in punti la finestra critica
	crit.point.values=round(crit.msec/(1000/samp.rate))
	exact.crit.msec=crit.point.values*(1000/samp.rate)
		
		allsubj_cond1=NULL
		for (i in 1:length(numbers1))
			{
			temp1a=eval(parse(file="", text=paste(base1,numbers1[i], sep="")),env=env)
			temp1b=apply(temp1a[msectopoint.values(win.ini, dim(temp1a)[1], baseline, total.length):msectopoint.values(win.end, dim(temp1a)[1], baseline, total.length),Electrodes], 1, mean) #medio per gli elettrodi
			allsubj_cond1=cbind(allsubj_cond1, temp1b)
			}
			

		if (!(is.null(base2))){
			allsubj_cond2=NULL
			for (i in 1:length(numbers2))
				{
				temp2a=eval(parse(file="", text=paste(base2,numbers2[i], sep="")),env=env)
				temp2b=apply(temp2a[msectopoint.values(win.ini, dim(temp1a)[1], baseline, total.length):msectopoint.values(win.end, dim(temp2a)[1], baseline, total.length),Electrodes], 1, mean) #medio per gli elettrodi
				allsubj_cond2=cbind(allsubj_cond2, temp2b)
				}
			}
			
			
			#### PARTE STATISTICHE
				
			t.result.values=NULL
			
			##### T.TEST - PAIRED == FALSE | PAIRED == TRUE E DUE PARAM ###
			if (paired==F|(paired==T&!(is.null(base2)))){
			for (i in 1:dim(allsubj_cond1)[1])	#prendo la lista 1, ma la lista 2 saraà lunga uguale (numero di 		punti)
				{
				t.result.values[i]=t.test(allsubj_cond1[i,], allsubj_cond2[i,], var.equal=T,paired=paired)$statistic 
				}	
			
			dfs=as.numeric(t.test(allsubj_cond1[i,], allsubj_cond2[i,], paired=paired)$parameter) #prendo i df dall'ultimo passo 

			lat.pos=nhigher(t.result.values, n=crit.point.values, criterion=qt(0.05, df=dfs,lower.tail=F))
			lat.neg=nhigher(-t.result.values, n=crit.point.values, criterion=qt(0.05, df=dfs,lower.tail=F))
			lat.pos.msec=win.ini+(lat.pos-1)*1000/samp.rate
			lat.neg.msec=win.ini+(lat.neg-1)*1000/samp.rate
			
			return(list(t.values=t.result.values, t.crit=qt(0.05, df=dfs), df=dfs,  crit.msec=crit.msec, exact.crit.msec=exact.crit.msec, exact.crit.point.values=crit.point.values, lat.pos.points=lat.pos, lat.neg.points=lat.neg, LATENCY.pos=lat.pos.msec, LATENCY.neg=lat.neg.msec))
			

			
			
						}
			
			##### T.TEST - PAIRED == TRUE , UN SOLO data.frame #### 
			### (ipotest H0,  M=0, sensato per differenziali)
			
			if (paired==T&is.null(base2)){
				for (i in 1:dim(allsubj_cond1)[1])	#prendo la lista 1, ma la lista 2 saraà lunga uguale (numero di 		punti)
				{
				t.result.values[i]=t.test(allsubj_cond1[i,], var.equal=T)$statistic ### 	PARTE DA 	MIGLIORARE. ATTUALMENTE PREVEDE SOLO DISEGNO BETWEEN. 
				}	

						
			dfs=t.test(allsubj_cond1[i,])$parameter #prendo i df dall'ultimo passo 
			
			lat.pos=nhigher(t.result.values, n=crit.point.values, criterion=qt(0.05, df=dfs, lower.tail=F))
			lat.neg=nhigher(-t.result.values, n=crit.point.values, criterion=qt(0.05, df=dfs,lower.tail=F))

			lat.pos.msec=win.ini+(lat.pos-1)*1000/samp.rate # nota il -1. 
			lat.neg.msec=win.ini+(lat.neg-1)*1000/samp.rate

			
			return(list(t.values=t.result.values, t.crit=qt(0.05, df=dfs, lower.tail=F), df=dfs, crit.msec=crit.msec, exact.crit.msec=exact.crit.msec, exact.crit.point.values=crit.point.values, lat.pos.points=lat.pos, lat.neg.points=lat.neg, LATENCY.pos=lat.pos.msec, LATENCY.neg=lat.neg.msec))

			}
		
		}
