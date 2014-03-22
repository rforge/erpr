erp.t <-
function(base1, base2,numbers1,numbers2=numbers1, electrode, paired=TRUE, alpha=0.05, envir=.GlobalEnv, envir2=NULL,sig=NULL, main=electrode, smo=0.5 , col="black", startmsec=-200, endmsec=1200, interval=c(startmsec, endmsec), step=200, verticals=NULL,horizontals=NULL, x.axis="default", ylim=c(-6,6), lwd=c(1,1), lty=c(1,1), cex.xaxis=0.8, cex.yaxis=1, color.list=c("blue", "red")) {


	#### PARTE 1: STATISTICHE PER ELETTRODO ####
if (is.null(numbers2)){
	numbers2=numbers1}

if (is.null(envir2)){
	envir2=envir}

if (is.null(sig)){
element=function(x,row.i){
	return(x[row.i,])
	}

alldata1.list=list(NULL)
alldata2.list=list(NULL)
for (i1 in 1:length(numbers1)){
	alldata1.list[[i1]]=eval(parse(file="", text=paste(base1,numbers1[i1], sep="")),envir=envir)
	}
for (i2 in 1:length(numbers2)){
	alldata2.list[[i2]]=eval(parse(file="", text=paste(base2,numbers2[i2], sep="")),envir=envir2)
	}


alltemp=list(NULL)
length(alltemp)=dim(alldata1.list[[1]])[1] #creo una lista con tanti elementi quanti i punti del tracciato.
alltemp.results=list(NULL)
length(alltemp.results)=dim(alldata1.list[[1]])[1] #creo una lista con tanti elementi quanti i punti del tracciato.


n.points.time=floor(seq(1,dim(alldata1.list[[1]])[1],dim(alldata1.list[[1]])[1]/10))
time.elapsed=0
cat("t-test results computation\n")
for (k in 1:dim(alldata1.list[[1]])[1]) {#prendo la dimensione di un data.frame qualsiasi
		temp1=lapply(alldata1.list, function(x) { element(x,k) } )
		temp1.1=matrix(unlist(temp1), ncol=length(alldata1.list[[1]]), byrow=TRUE)
		temp2=lapply(alldata2.list, function(x) { element(x,k) } )
		temp2.1=matrix(unlist(temp2), ncol=length(alldata1.list[[1]]), byrow=TRUE) #di ciascuna riga una 
		#matrice con n righe (una per soggetto) e k colonne (una per elettrodo) 
		alltemp[[k]][[1]]=temp1.1
		alltemp[[k]][[2]]=temp2.1
		temp.results.vet=NULL
		for (j in 1:dim(alltemp[[k]][[1]])[2]){#nota:uso dim perché alltemp[[k]][[1]] è una matrice
		temp.results.vet[j]=(t.test(alltemp[[k]][[1]][,j], alltemp[[k]][[2]][,j], corr=F, paired=paired)$p.value)<alpha
		}
		alltemp.results[[k]]=temp.results.vet
		if (k%in%n.points.time){
			cat(rep(".",10-time.elapsed), "\n")
			time.elapsed=time.elapsed+1
			}
		}
		cat("\n")

alltemp.results=matrix(unlist(alltemp.results), byrow=TRUE, ncol=dim(alldata1.list[[1]])[2])
alltemp.results=as.data.frame(alltemp.results)
names(alltemp.results)=names(alldata1.list[[1]])
		}
		
if (!is.null(sig)){
	alltemp.results=sig
	}

##### PARTE 2 CREO DATAFRAME PER SCALP




alldata1=grandaverage(base=base1, numbers1, envir=envir)
alldata2=grandaverage(base=base2,numbers2, envir=envir2)



e1=alldata1[,electrode]
e2=alldata2[,electrode]

lengthwhole=length(e1)
	
	startpoint=msectopoints(interval[1], lengthwhole)
	endpoint=msectopoints(interval[2], lengthwhole)
	
	
	vet=seq(interval[1], interval[2], step)
	
	if (x.axis[1]!="default"){
		vet=x.axis
		}
		
	temp0=msectopoints(0, lengthwhole)
	vet2=msectopoints(vet, lengthwhole)
	vet.names=paste(vet) # vet sarebbero le labels del nuovo asse

	maxe1=max(e1)
	mine1=min(e1)
		

		plot(e1, type="n", ylim=ylim, col=col,lwd=lwd, main=main, xaxt="n",xlim=c(startpoint, endpoint), ylab="",xlab="", lty=lty, cex.axis=cex.yaxis)
		
		# plotto le bande di significatività
		######################
		abline(v=grep(TRUE, alltemp.results[,electrode]), col="lightgray", lwd=3)
		#######################

		
		lines(smooth.spline(e1, spar=smo), col=color.list[1],lwd=lwd[1], type="l", lty=lty[1])
		
		lines(smooth.spline(e2, spar=smo), col=color.list[2],lwd=lwd[2], type="l", lty=lty[2])

	
		axis(1,vet2, paste(vet), cex.axis=cex.xaxis)
		abline(h=0, lty="longdash")
		segments(temp0,-0.5,temp0,0.5, lty=1)
	
	# draw vertical lines (expressed in msec and authomatically converted in points)
	for (i in 1:length(verticals)){
		x=msectopoints(verticals[i], lengthwhole)
		abline(v=x)
		}	
	#draw horizontal lines (expressed in microvolts)
	for (i in 1:length(horizontals)){
		x=(horizontals[i])
		abline(h=x)
	}	

print(color.list)
invisible(alltemp.results)
}
