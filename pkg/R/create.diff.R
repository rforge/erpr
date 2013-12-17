create.diff <-
function(base1, base2, numbers, outname=NULL, env=.GlobalEnv)
	{
	if (is.null(outname))
	{
	stop("devi specificare outname!!")
	}
	for (i in 1:length(numbers))
	{
	assign(paste(outname,numbers[i], sep=""),(eval(parse(file="", text=paste(base1,numbers[i], sep="")),env=env))-eval(parse(file="", text=paste(base2,numbers[i], sep="")),env=env),env=env)
	}
	}
