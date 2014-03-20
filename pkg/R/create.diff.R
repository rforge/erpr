create.diff <-
function(base1, base2, numbers, outname=NULL, envir=.GlobalEnv, out.envir=envir, comment.dat=1)
	{
	if (is.null(outname))
	{
	stop("devi specificare outname!!")
	}
	bases=c(base1, base2)
	for (i in 1:length(numbers))
	{
		temp.out=eval(parse(file="", text=paste(base1,numbers[i], sep="")),envir=envir)-eval(parse(file="", text=paste(base2,numbers[i], sep="")),envir=envir)
		comment(temp.out)=comment(eval(parse(file="", text=paste(bases[comment.dat],numbers[i], sep="")),envir=envir))
		assign(paste(outname,numbers[i], sep=""), temp.out, envir=out.envir) 
	}
}
