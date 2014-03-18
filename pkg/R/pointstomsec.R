pointstomsec=function(a, lengthsegment, startmsec, endmsec)
{
	totmsec=endmsec-(startmsec) #total duration in msec
	pointsstep=totmsec/lengthsegment #how many msec is a point.
	x=(a*pointsstep)+(startmsec)
	return(x)

}
