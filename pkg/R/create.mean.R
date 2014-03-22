create.mean <-
function(bases, numbers, outname=NULL, envir=.GlobalEnv, out.envir=envir, fileinfo=1)
	{
	if (is.null(outname))
	{
	stop("devi specificare outname!!")
	}
	for (i in 1:length(numbers))
	{
	temp=0 #creo inizialmente questo oggetto che mi serve da oggetto vuoto per il ciclo che segue.
	for (k in 1:length(bases))
		{
		temp=eval(parse(file="", text=paste(bases[k],numbers[i], sep="")),envir=envir)+temp
		}
		temp.out=(temp/length(bases))
		comment(temp.out)=comment(eval(parse(file="", text=paste(bases[fileinfo],numbers[i], sep="")),envir=envir))
		assign(paste(outname,numbers[i], sep=""),temp.out, envir=out.envir)		
	}
	}
