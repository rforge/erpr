ezresults <-
function(mod, filter=TRUE, digit=2)
	{
	if(length(mod)==1)
	{
	anovares=mod[[1]]
	res=anovares[anovares[,"p<.05"]=="*",]
	}
	if (length(mod)>1){
	anovares=mod[[1]]
	anovasph=mod[[2]]
	anovacorr=mod[[3]]
	
	if (filter==TRUE){
	anovares=anovares[anovares[,"p<.05"]=="*",]
	}
		
	anovares[,c(4,5)]=round(anovares[,c(4,5)],2)
	anovares[,7]=signif(anovares[,7],digit)
	
	
	anovasph=anovasph[rownames(anovasph)%in%rownames(anovares),]
	
	res=anovares
	
	
		if (dim(res)[1]==0){
	cat("nessun effetto significativo\n")	
	#nota: non utilizzo semplicemente la funzione "stop" altrimenti si bloccherebbe la funziona ezANOVA.nout.
	}
	
	
	if (dim(res)[1]!=0){
	res$sphericity.sig=""
	res[rownames(anovares)%in%rownames(anovasph),"sphericity.sig"]=anovasph[,"p<.05"]
	
	
	anovacorr=anovacorr[rownames(anovacorr)%in%rownames(anovasph),]
	
	res$p.corrGG=""
	res$sig.GG=""
	res$p.corrHF=""
	res$sig.HF=""
	
	res[rownames(anovares)%in%rownames(anovacorr), "p.corrGG"]=signif(anovacorr[,"p[GG]"],digit)
	res[rownames(anovares)%in%rownames(anovacorr), "sig.GG"]=anovacorr[,"p[GG]<.05"]

	res[rownames(anovares)%in%rownames(anovacorr), "p.corrHF"]=signif(anovacorr[,"p[HF]"],digit)
	res[rownames(anovares)%in%rownames(anovacorr), "sig.HF"]=anovacorr[,"p[HF]<.05"]

	return(res)
	}
	}
	if (length(mod)==1)	
	return(res)
	}
