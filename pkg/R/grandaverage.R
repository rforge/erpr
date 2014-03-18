grandaverage <-
#NOTA questa funzione può indurre in errore, perché ci sono NA

# in questa f(x) AGGIUNGI UN WARNING SE CI SONO NA e crea una funzione (da usare preliminarmente) che faccia un check di tutti i dati
# magari una funz veloce che ti dica solo se sono completi e una più dettagliata che ti dica invece range e eventuali NA.

function(base, numbers, electrodes="all", env=.GlobalEnv, NA.sub=T)
	#forse da togliere la possibilità di selezionare elettrodi.
	{
	comment_text=paste("Subjects averaged: ", paste(base,numbers[1], sep=""))
	average.temp=eval(parse(file="", text=paste(base,numbers[1], sep="")),env=env)
	
	if(electrodes[1]=="all"){
		electrodes=names(average.temp)
	}
	
	average.temp=average.temp[,electrodes]
	noNA.num=apply(average.temp, 2, function(x){as.numeric(!all(is.na(x)))})
	
	if(NA.sub==TRUE)
			{
				average.temp[is.na(average.temp)]=0
			}

	
		for (i in 2:length(numbers))
		{
			average.temp.new=eval(parse(file="", text=paste(base,numbers[i],sep="")),env=env)[,electrodes]
			
			if(NA.sub==TRUE)
			{
				average.temp.new[is.na(average.temp.new)]=0
			}
			
			average.temp=average.temp+average.temp.new
			
			comment_text=paste(comment_text,paste(base,numbers[i], sep=""),"\n")
			
			#noNA.num.new=sum(as.numeric(is.na(average.temp.new)))
			noNA.num.new=apply(average.temp.new, 2, function(x){as.numeric(!all(is.na(x)))})
			noNA.num=rbind(noNA.num,noNA.num.new)			
			
		}
		Electrodes.n=colSums(noNA.num) # Electrodes.n è il numero di soggetti per cui gli elettrodi non hanno NA
		average=average.temp/rep(Electrodes.n, each=nrow(average.temp))
		comment(average)=comment_text
		if (sum(Electrodes.n-(length(numbers)))!=0){ #nota: length(numbers) è il numero di soggetti. In questo modo recupero il numero di sogg con NA.
			warning("The average included some NA values. Use check.erp function", call.=FALSE)
		}
		return(average)
		}
