### ms.locator ###

ms.locator<-function(type = "p", startmsec, endmsec, lengthsegment, label=TRUE, n=512, ...){
	for (i in 1:n){
	coord=locator(1, type=type, ...)
	ms.coord=pointstomsec(coord$x, lengthsegment, startmsec, endmsec)
	print(ms.coord)
	if (label==TRUE){
		text(coord$x, coord$y, pos=3, labels=round(ms.coord)) #pos = 3 indicates to plot above the coorindate
	}
	}
}
