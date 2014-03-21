erp <-
function(e1,main=NULL, smo=0 , col="black", startmsec=-200, endmsec=1000, interval=c(startmsec, endmsec), step=200, verticals=NULL, horizontals=NULL, x.axis="default", ylim="default", lwd=1, lty=1, cex.main=1, cex.xaxis=0.8, cex.yaxis=1, y.axis="default", frame.plot=TRUE, xaxis0=FALSE, xtick.l="default",xticks="default", yaxis0=FALSE, yaxis0.origin=0, ytick.l="default", y.axis.step=2,  yticks="default", lwd.xticks=1, lwd.yticks=1){
	
	
	lengthwhole=length(e1)	
	
	startpoint=msectopoints(interval[1], lengthwhole, startmsec, endmsec)
	endpoint=msectopoints(interval[2], lengthwhole, startmsec, endmsec)
	
	
	vet=seq(interval[1], interval[2], step)
	
	if (x.axis[1]!="default"){
		vet=x.axis
		}
	
	#AGGIUNGI UNA CONDIZIONE IF: nel caso in cui il vettore specificato è particolarmente corto allora cambia la scala (a passi da 100 invece che a passi da 200.)
	
	
	
	temp0=msectopoints(0, lengthwhole, startmsec, endmsec)
	vet2=msectopoints(vet, lengthwhole, startmsec, endmsec)
	vet.names=paste(vet) # vet sarebbero le labels del nuovo asse

	maxe1=max(e1)
	mine1=min(e1)
	
	
	if (ylim[1]=="default"){
		ylim=sort(range(c(-6,6,maxe1, mine1)))
		}
	else {
		ylim=ylim
	}

	
		if(smo!=0){
		e1=smooth.spline(e1, spar=smo)
		}
		
		plot(e1, type="l", col=col,lwd=lwd, main=main, yaxt="n", xaxt="n",xlim=c(startpoint, endpoint), ylab="",xlab="", lty=lty, cex.axis=cex.yaxis, frame.plot=frame.plot, cex.main=cex.main, ylim=ylim)		
		
	
	
	if(xtick.l=="default"){
		xtick.l=sum(abs(ylim)/40)
		}
		
	
	
	if (xaxis0==FALSE){
		axis(1,vet2, paste(vet), cex.axis=cex.xaxis)
		abline(h=0, lty="longdash")
		segments(temp0,-y.axis.step/8,temp0,+y.axis.step/8, lty=1.5)
		}
		
	if (xticks[1]=="default"){
	xticks=vet	
	}
		
	if (xaxis0==TRUE){
		xticks=msectopoints(xticks, length(e1), startmsec, endmsec)	
		segments(xticks, -xtick.l, xticks, +xtick.l, lty=1.5, lwd=lwd.xticks)
		abline(h=0)
		}
	
	if (yaxis0==FALSE){
	if (ylim[1]<ylim[2]){
	axis(2,seq(ylim[1], ylim[2], y.axis.step), seq(ylim[1], ylim[2], y.axis.step), cex.axis=cex.xaxis)
	}
	if (ylim[1]>ylim[2]){
	axis(2,seq(ylim[2], ylim[1], y.axis.step), seq(ylim[2], ylim[1], y.axis.step), cex.axis=cex.xaxis)
	}
	}
	
	 if(ytick.l=="default"){
	 	ytick.l=length(e1)/40 #
	 }
	
	if (yticks[1]=="default") {
		yticks=seq(min(ylim), max(ylim[2]), y.axis.step)
	}
	
	if (yaxis0==TRUE){
	
		abline(v=msectopoints(yaxis0.origin, length(e1), startmsec, endmsec), lty=1.5)
		segments(msectopoints(yaxis0.origin, length(e1), startmsec, endmsec)-ytick.l, yticks, msectopoints(yaxis0.origin, length(e1), startmsec, endmsec)+ytick.l, yticks, lwd=lwd.yticks )
	}
	
	

	
	# draw vertical lines (expressed in msec and authomatically converted in points)
	for (i in 1:length(verticals)){
		x=msectopoints(verticals[i], lengthwhole, startmsec, endmsec)
		abline(v=x)
		}	
	#draw horizontal lines (expressed in microvolts)
	for (i in 1:length(horizontals)){
		x=(horizontals[i])
		abline(h=x)
	}	
}
