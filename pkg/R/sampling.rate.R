sampling.rate<-function(x, startmsec=NULL, endmsec=NULL){
  #### simple function to calculate sampling rate (in Hz) from object normally used in erpR package
	samp.rate=((dim(x)[1]-1)*1000)/(-startmsec+endmsec)
	return(samp.rate)
}