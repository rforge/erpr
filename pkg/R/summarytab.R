summarytab <-
function(deps, groups,na.rm=TRUE, substr.l=max(nchar(levels(groups)))){
	deps=as.data.frame(deps)

	groups=apply(as.data.frame(groups), 1, function(x){paste(x,collapse="_")})
	groups=as.factor(groups)
	levels(groups)=substr(levels(groups), 1,substr.l)

	for (i in 1:length(deps)){
		sum.Ntot=as.numeric(table(groups))
		sum.Nvalid=as.numeric(table(groups[!is.na(deps[,i])]))
		sum.NA=as.numeric(table(groups[is.na(deps[,i])]))
		sum.mean=tapply(deps[,i], groups, mean, na.rm=na.rm)
		sum.sd=tapply(deps[,i], groups, sd,na.rm=na.rm)
		sum.max=tapply(deps[,i], groups, max,na.rm=na.rm)
		sum.min=tapply(deps[,i], groups, min,na.rm=na.rm)
		
	
	results=data.frame(MEAN=as.numeric(sum.mean), SD=as.numeric(sum.sd), MIN=as.numeric(sum.min), MAX=as.numeric(sum.max), Ntot=sum.Ntot, Nvalid=sum.Nvalid, NotAv=sum.NA)
	rownames(results)=levels(groups)
	#return(results)
	cat("--------------------------\n")
	cat("--", names(deps)[i],"--\n")
	print(results)
	cat("--------------------------\n")
	cat("\n\n")
	}
}
