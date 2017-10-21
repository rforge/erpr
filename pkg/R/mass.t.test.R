mass.t.test<-function(base1=NULL, base2=NULL, numbers1=NULL, numbers2=numbers1, startmsec=NULL, endmsec=NULL, paired=F, erplist1=NULL, erplist2=erplist1,  electrodes="all", to.exclude=NULL, interval=c(startmsec, endmsec), p.adjust.method="none", n.permutations=1000, p.crit=0.05, stable.diff=FALSE, crit.msec=NULL, crit.npoints=NULL, neighbours=NULL, na.rm=TRUE) {

# NOTE: this is mass.t.test version 5
# cluster based permutation currently not implemented

# preliminary checks
	if (is.null(erplist1)|is.null(erplist2)){
	stop("two erplist objects (erplist1 and erplist2) containing ERP data frames must be specified!", call.=F)
	}
	
	# consistency checks for paired =T
	if (paired==TRUE&(length(numbers1)!=length(numbers2))){
	stop ("if paired == TRUE, numbers1 and numbers2 must have equal length.", call.=F)
	}
	
	#### object checks
	object.names1=paste(base1, numbers1, sep="")
	if (any(!object.names1%in%names(erplist1))){
		missing.objects1=object.names1[!object.names1%in%names(erplist1)]
		missing.object.collist1=paste(missing.objects1, "\n", sep="")
		stop("The following objects are not contained in the erplist1 specified:\n", missing.object.collist1, call.=F)
	}
		#### object checks
	object.names2=paste(base2, numbers2, sep="")
	if (any(!object.names2%in%names(erplist2))){
		missing.objects2=object.names2[!object.names2%in%names(erplist2)]
		missing.object.collist2=paste(missing.objects2, "\n", sep="")
		stop("The following objects are not contained in the erplist2 specified:\n", missing.object.collist2, call.=F)
	}
	




###
# STEP 1 LOAD DATA
#

# retrieve names from first subject.
if (electrodes[1]=="all"){
	electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])
	}

### to.exclude overwrite electrodes!
if (!is.null(to.exclude)){
	electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])[!names(erplist1[[paste(base1, numbers1[1], sep = "")]])%in%to.exclude]
	}

### select interval to be analyzed.
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[2], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
new.dim1=length(startpoint:endpoint)

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)



# datall is a matrix with subject in columns and time x electrodes in rows.

datall1 = NULL
    for (i in 1:length(numbers1)) {
        x.temp = erplist1[[paste(base1, numbers1[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
        
        # add exception if only one electrode is included
        if(is.null(dim(x.temp)[1])){
        	x.temp=data.frame(x.temp)
        	names(x.temp)=electrodes
        }
        
		x.vec=unlist(x.temp)
		if (i == 1) {
	        datall1 = x.vec
	        }
        if (i != 1) {
    		datall1=cbind(datall1, x.vec)
        }
    }
    
datall2 = NULL
    for (i in 1:length(numbers2)) {
        x.temp = erplist2[[paste(base2, numbers2[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
		
		# add exception if only one electrode is included
        if(is.null(dim(x.temp)[1])){
        	x.temp=data.frame(x.temp)
        	names(x.temp)=electrodes
        }

		
		x.vec=unlist(x.temp)
		if (i == 1) {
	        datall2 = x.vec
	        }
        if (i != 1) {
    		datall2=cbind(datall2, x.vec)
        }
    }

## 
# STEP 2 T.TEST POINT by POINT
###    

if (!p.adjust.method%in%c("tmax", "cluster.based.permutation")){
  
allres=rep(NA, dim(datall1)[1]) # uso datall1 come riferimento ma la dimensione (i timepoints) saranno uguale a datall2
allres.t=rep(NA, dim(datall1)[1]) # creo matrice per valori di t

if (paired ==  FALSE) {


	for (i in 1:dim(datall1)[1]){
	  
	# res=t.test(datall1[i,], datall2[i, ], var.equal=T, paired=F)$p.value
	  
	df=length(datall1[i, ]) -1 + length(datall2[i, ]) -1  # remember that cols of datall are the subjects.
	t.crit=qt(p.crit/2, df=df) # non lo uso per ora. Non sono manco sicuro sia giusto
	
	res.t=(mean(datall1[i, ], na.rm=T)-mean(datall2[i, ], na.rm=T))/sqrt(var(datall1[i,], na.rm=T)/length(datall1[i,])+var(datall2[i, ], na.rm=T)/length(datall2[i,])) # independent samples.
	
	res=pt(abs(res.t), df=df, lower.tail=F)*2
	
	allres[i]=res #p-value
	
	allres.t[i]=res.t

	}
}

if (paired == TRUE) {
  
  #df=length(datall1[i, ]) -1 

	for (i in 1:dim(datall1)[1]){
	
	df= sum(!is.na(datall1[i, ])) - 1 
	t.crit=qt(p.crit/2, df=df) # non lo uso per ora.
	
	res.t=mean((datall1[i, ]-datall2[i, ]), na.rm = na.rm)/sqrt((var(datall1[i, ]-datall2[i, ], na.rm=na.rm)/length(datall1[i, ])))
	# dependent samples
	
	res=pt(abs(res.t), df=df, lower.tail=F)*2

	
	allres[i]=res
	
	
	allres.t[i]=res.t

	}
}

allres=p.adjust(allres, method=p.adjust.method)

} # end t.test for normal condition

if (p.adjust.method=="tmax"){

# permutations are made from datall1 and datall2

	if (paired == FALSE){

	# creo data.frame con tutti i soggetti. serve solo se paired=F
	datall12=cbind(datall1, datall2)
	# creo vettore con numero totale di soggetti
	all.numbers=1:dim(datall12)[2] 

	

	allres.t.obs=rep(NA, dim(datall1)[1]) # creo matrice per valori di t

	t.max.res=rep(NA, n.permutations)
	
	for (i in 1:dim(datall1)[1]){
	
	res.t.obs=(mean(datall1[i, ])-mean(datall2[i, ]))/sqrt(var(datall1[i,])/length(datall1[i,])+var(datall2[i, ])/length(datall2[i,])) # independent samples.
	
		
	allres.t.obs[i]=res.t.obs

	}
		# set timer for permutations:
		n.points.time=floor(seq(1, n.permutations, n.permutations/10))
		time.elapsed=0
		
		# set length outside the for loop to speed up computation.
		# I will need them inside the loop for i in 1:n.permutations
		datall1.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
		datall2.perm.len=rep(dim(datall2)[2], dim(datall2)[1])
		
		cat("computing permutations:\n")
		
		for (i in 1:n.permutations){
		
		perm.index1=sample(all.numbers, length(numbers1))
		datall1.perm=datall12[,perm.index1]
		datall2.perm=datall12[,-perm.index1]
		
		curr.perm.t=rep(NA, dim(datall1.perm)[1])

		
		
			
			datall1.perm.mean=rowMeans(datall1.perm)
			datall2.perm.mean=rowMeans(datall2.perm)
			datall1.perm.var=rowSums((datall1.perm-datall1.perm.mean)^2)/(datall1.perm.len)
			datall2.perm.var=rowSums((datall2.perm-datall2.perm.mean)^2)/(datall2.perm.len)

			
			curr.perm.t=(datall1.perm.mean-datall2.perm.mean)/sqrt(datall1.perm.var/datall1.perm.len+datall2.perm.var/datall2.perm.len)
			

			
			if (i%in%n.points.time){
			cat(rep(".",10-time.elapsed), "\n")
			time.elapsed=time.elapsed+1
			}
		
		t.max.res[i]=max(abs(curr.perm.t))

	}

	p.corr=sapply(allres.t.obs, function(x){(sum(as.numeric(t.max.res>=abs(x)))/length(t.max.res))})
allres=p.corr
allres.t=allres.t.obs

} # end if method = tmax paired =F	
	




	
	if (paired == TRUE) {
		
	# datall12 is the dataframe with the difference of datall1 and datall2.
	datall12=datall1-datall2
	allres.t.obs=rep(NA, dim(datall12)[1])
		
	for (i in 1:dim(datall12)[1]){

	res.t.obs=mean(datall12[i, ])/sqrt((var(datall12[i,])/length(datall12[i, ])))
		
	allres.t.obs[i]=res.t.obs

	}
	
	# create object with all t.max
	t.max.res=rep(NA, n.permutations)
	
	# create object with length to speed-up computation
	datall.perm.len=rep(dim(datall1)[2], dim(datall1)[1])

	
	# set timer for permutations:
	n.points.time=floor(seq(1, n.permutations, n.permutations/10))
	time.elapsed=0
	cat("computing permutations:\n")
	
	for (i in 1:n.permutations){
		
		
	perm=sample(c(1,-1), length(numbers1), replace=T)
		
	datall.perm=t(t(datall12)*perm)
	
	datall.perm.mean=rowMeans(datall.perm)
	datall.perm.var=rowSums((datall.perm-datall.perm.mean)^2)/(datall.perm.len)

	curr.perm.t=datall.perm.mean/sqrt(datall.perm.var/datall.perm.len)
		
			
	t.max.res[i]=max(abs(curr.perm.t))
	
	if (i%in%n.points.time){
		cat(rep(".",10-time.elapsed), "\n")
		time.elapsed=time.elapsed+1
		}	
	}
	
	p.corr=sapply(allres.t.obs, function(x){(sum(as.numeric(t.max.res>=abs(x)))/length(t.max.res))})
	allres=p.corr
	allres.t=allres.t.obs
} # end if method = tmax, paired = T

} # end if method = tmax


##############################
# reconstruct the matrix to return the reuslts
##############################
# the following part is the same for simple t.tests and tmax permutation
# I write it here to avoid replicating code.
# the results may also be fed to the stable.diff fun.
# NOTE: that the following code cannot be used for cluster based permutation
# in which negative and positive results are filtered separately

## filtro i risultati  di res.t.mat a partire da crit.mat e riarrangio al volo.
res.mat=matrix(allres, nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)
res.t.mat=matrix(allres.t, nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)
res.p.mat=matrix(allres, nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)
colnames(res.t.mat)=names(x.temp)


#### 


res.mat.filter=matrix(NA, nrow=dim(res.mat)[1], ncol=dim(res.mat)[2])
res.log.mat=res.mat<p.crit
res.mat.filter[res.log.mat]=TRUE

filt.mat=matrix(res.t.mat[res.mat.filter], nrow=nrow(res.mat), ncol=ncol(res.mat))

## reconstruct a data.frame with the original number of time points before returning results
# "or" prefix indicates that the dimensions are referred to the original data dimensions
or.filt.mat=as.data.frame(matrix(rep(NA, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2]))

# IMPORTANT! here the matrix is compsed by FALSE or TRUE (no NA) to be used properly with stable.diff
or.res.log.mat=as.data.frame(matrix(rep(FALSE, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2])) # creo lo stesso per risultati log
or.res.t.mat=or.filt.mat # the same for t.
or.res.p.mat=or.filt.mat # and for p

or.filt.mat[startpoint:endpoint, ]=filt.mat
or.res.log.mat[startpoint:endpoint, ]=res.log.mat
or.res.t.mat[startpoint:endpoint, ]=res.t.mat
or.res.p.mat[startpoint:endpoint, ]=res.p.mat


colnames(or.filt.mat)=names(x.temp)
colnames(or.res.log.mat)=names(x.temp)
colnames(or.res.t.mat)=names(x.temp)
colnames(or.res.p.mat)=names(x.temp)

#########################################
####### CALCULATE EXACT MS OF INTERVAL
#########################################
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
new.dim1=length(startpoint:endpoint)

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)

############################################
# STABLE DIFF
############################################
if (stable.diff==TRUE){

####### CALCULATE CRIT.MSEC (for stable.diff)

  mysamp.rate=sampling.rate(erplist1[[paste(base1, numbers1[1], sep = "")]], startmsec=startmsec, endmsec=endmsec)
  
  if (!is.null(crit.msec)){
    crit.npoints=round(crit.msec/(1000/mysamp.rate))
  }
  
  # calculate the exact ms, to be returned in param
  exact.crit.msec=crit.npoints*(1000/mysamp.rate)

  stable.diff.res=stable.diff.fun(or.res.log.mat, electrodes, crit.npoints, interval, startmsec, endmsec)
  
  # "filter" again the data, on the basis of stable.diff.fun results
  
  #print(str(stable.diff.res, 1))
  or.filt.mat[!stable.diff.res$res.log.mat]=NA
  
  
} # end if (stable.diff==TRUE)

#######################################
#### CLUSTER BASED PERMUTATION
#######################################

## DA INSERIRE QUI

# fine clust.based permutation

### cambio i risultati NA in 0 ###

#or.filt.mat[is.na(or.filt.mat)]=0
# questo mi serve temporaneamente per usare mass.t


param=data.frame( interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1], total.n.test=length(allres), p.correction=p.adjust.method)

if (stable.diff==TRUE){
  param=cbind(param, crit.msec=crit.msec, exact.crit.msec=exact.crit.msec)
}


allresults=list(param=param, mass.t.results=or.filt.mat, sig=or.res.log.mat, t.mat=or.res.t.mat, p.mat=or.res.p.mat)

if (stable.diff==TRUE){
  allresults=c(allresults, crit.list=list(stable.diff.res$crit.list))
}


invisible(allresults)

}



