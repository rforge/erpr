export.bplot <-
function(dat, Npts=dim(dat)[1], startmsec=-200, endmsec=1200, SB=1, SC=500, outname="Bplotfile.avr")
	{
	TSB=startmsec
	DI=(endmsec+abs(startmsec))/Npts # distanza in ms tra un punto e l'altro della sweep.
	dat=t(dat)
	write(paste("Npts=",Npts, "TSB=",TSB, "DI=",DI,"SB=",SB, "SC=",SC, sep=" "), file=outname)
	write.table(dat, row.names=F, col.names=F, file=outname, append=TRUE)
	}
