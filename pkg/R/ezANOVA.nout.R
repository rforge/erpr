ezANOVA.nout <-
function(data, out, n, p.crit=0.05, plot=FALSE, plot.par=NULL, cex.x, plot.vars="all", ...) {
	
	out=factor(data[,paste(out)])
	out.s=combn(levels(out), length(levels(out))-n)
	cat("  ", dim(out.s)[2], "possibile combinations will be evaluated\n")
	counter=0
	for (i in 1:dim(out.s)[2]){
		dat=data[out%in%out.s[,i],]
		dat=factorall(dat)
		res=ezANOVA(data=dat, ...) #res=ezANOVA(data=dat, dv=dv, wid=wid, within=within) #
		
		res.sig=ezresults(res)
		
			if(any(res.sig$p<p.crit)){
				counter=counter+1
				if (counter==1){
					res.list=list(list(out=levels(out)[!levels(out)%in%out.s[,i]],res.sig))
				}
				if (counter>1){
					res.list[[counter]]=list(levels(out)[!levels(out)%in%out.s[,i]],res.sig)
				}
			}
		
	}
	alleffects=res$ANOVA$Effect
	attr(res.list, "effects")=alleffects
	attr(res.list, "out")=out

	if (plot==TRUE){
	
	if (plot.vars!="all"){
	alleffects=plot.vars
	}
	
	
	if (!is.null(plot.par)){
		par(mfrow=c(plot.par))} else {
			par(mfrow=c(1,length(alleffects)))
			}

	
	for (k in 1:length(alleffects)){
		eff=lapply(res.list, function(x) {grep(paste("^",alleffects[k],"$", sep=""), x[[2]]$Effect)})
		list.sig=res.list[!is.na(eff>1)] #recupero le sottoliste in cui è presente l'effetto. NOTA: dal momento che ho utilizzato ezresults, l'effetto è presente solo se significativo. Nota che però, per come è fatta la condizione logica if (any)
		list.sig.out=lapply(list.sig, function(x){ x[[1]]})
		sig.out=unlist(list.sig.out)
		table.sig.out=table(sig.out)
		add.names=levels(out)[!levels(out)%in%names(table.sig.out)]
		add.matrix=matrix(0, ncol=length(add.names), nrow=1)
		if (dim(add.matrix)[2]>0){
		final.matrix=cbind(add.matrix, t(matrix(table.sig.out))) #nota: il fatto che c'è t matrix è per una corretta combinazione di add.table e table.sig.out
		colnames(final.matrix)=c(add.names, names(table.sig.out))
		}
		if (dim(add.matrix)[2]==0){
		final.matrix=t(matrix(table.sig.out))
		colnames(final.matrix)=names(table.sig.out)
		}
		barplot(final.matrix, main=alleffects[k], col="gray", cex.names=cex.x)	
		}
	}

 return(res.list)#, matrix(unlist(list.sig.out), ncol=n,byrow=TRUE)))
 
}
