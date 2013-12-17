grandaverage <-
function(base, numbers, env=.GlobalEnv)
	{
	comment_text=paste("Subjects averaged: ", paste(base,numbers[1], sep=""))
	average.temp=eval(parse(file="", text=paste(base,numbers[1], sep="")),env=env)
		for (i in 2:length(numbers))
		{
			average.temp=average.temp+eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
			comment_text=paste(comment_text,paste(base,numbers[i], sep=""),"\n")
		}
		average=average.temp/length(numbers)
		comment(average)=comment_text
		return(average)
		}
