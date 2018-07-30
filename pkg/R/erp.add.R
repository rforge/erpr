erp.add <-
function(erpdf, electrode, startmsec=NULL, endmsec=NULL, interval=NULL, smo=NULL,  col="black", lty=1, lwd=1, ...){
	
  # get startmsec from the first object
  if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
    startmsec=attr(erpdf, "startmsec")
    endmsec=attr(erpdf, "endmsec")
  }
  
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  if (is.null(interval)){
    interval=c(startmsec, endmsec)
  }
  
  if (!electrode%in%names(erpdf)){
    stop("the electrode to plot is not present in the erpdf", call.=F)
  }
  
  # determine what to plot
  el = erpdf[, electrode]
  
  
  
  if (!is.null(smo)){
		el=smooth.spline(el, spar=smo)$y #effettuo un po' di smoothing sul segnale
	}
	# first I calculate the msectopoints on the WHOLE length of the data
	lengthwhole=length(el)	
	
	startpoint=par("usr")[1]
	endpoint=par("usr")[2]
	
	# determine the waveform ot plot according to interval
	plotinterval=msectopoints(interval[1], lengthwhole, startmsec, endmsec): msectopoints(interval[2], lengthwhole, startmsec, endmsec )
	
	
	if (!is.null(smo)){
		el=smooth.spline(el, spar = smo)$y
	}
	
	### PLOT WAVEFORM
	# notice the default xlim is from 1 to the whole length of the data (not in intervals)
	
	lines(plotinterval, el[plotinterval], col=col, lty=lty, lwd=lwd, ...)
	}
