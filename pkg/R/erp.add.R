erp.add <-
function(en, lty=1, smo=0, col="black", lwd=1, ...){
	if (smo!=0){
		en=smooth.spline(en, spar=smo) #effettuo un po' di smoothing sul segnale
	}
	lines(en, col=col, lwd=lwd, lty=lty, ...)
	}
