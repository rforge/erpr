### ms.locator ###

ms.locator<-function(type = "p", startmsec=NULL, endmsec=NULL, lengthsegment, label=TRUE, n=512, ...){
  
  # check for startmsec and endmsec
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  for (i in 1:n){
    coord=locator(1, type=type, ...)
    ms.coord=pointstomsec(coord$x, lengthsegment, startmsec, endmsec)
    print(ms.coord)
    if (label==TRUE){
      text(coord$x, coord$y, pos=3, labels=round(ms.coord)) #pos = 3 indicates to plot above the coorindate
    }
  }
}
