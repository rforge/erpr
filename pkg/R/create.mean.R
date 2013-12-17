create.mean <-
function(bases, numbers, outname=NULL, env=.GlobalEnv, out.env=env, comment.dat=1)
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
		temp=eval(parse(file="", text=paste(bases[k],numbers[i], sep="")),env=env)+temp
		}
		temp.out=(temp/length(bases))
		comment(temp.out)=comment(eval(parse(file="", text=paste(bases[comment.dat],numbers[i], sep="")),env=env))
		assign(paste(outname,numbers[i], sep=""),temp.out, env=out.env)		
	}
	}
