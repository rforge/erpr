#in variables se c'è caudalità e lateralità devi specificare due colonne! es. plotsetup(type.setup[,1], type.setup[,c(2,3)])


plotsetup=function(electrodes, variables=NULL)
	{
	
	par(mfrow=c(7,5), mai=c(0,0,0,0))
	
	all.electrodes=c("", "Fp1", "Fpz", "Fp2", "", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCz", "FC4", "FT8", "T3", "C3", "Cz", "C4", "T4", "TP7", "CP3", "CPz", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "", "O1", "OZ", "O2", "")
	
	color.list=c(palette(),colours()[2:10])
	
	
	if (!is.null(variables))
	{
	variables=data.frame(variables)
	#trasformo in character qualora non lo fossero
	for (i in 1:length(variables))
		{
		variables[,i]=as.character(variables[,i])
		}
	
	new.var=variables[,1] #creo la nuova variabile
		
	if (dim(variables)[2]>1)
		{
		for (i in 2:length(variables))
			{
			new.var=paste(new.var, variables[,i])
			}
		}
	
	new.var=as.factor(new.var)	
	setup=data.frame(electrodes=electrodes, new.var=new.var)
	}
	
	electrode.col=all.electrodes%in%electrodes
	
	x=c(0,0,1,1) # creo x e y del quadrato
	y=c(0,1,1,0)
	
	for (i in 1:length(all.electrodes))
		{
		plot.new()
		### se non sono specificate variabili
		if (is.null(variables))
			{
			if (electrode.col[i]==TRUE)
				{
				polygon(x,y, col="gray")
				}
			}
		if (!is.null(variables))
			{
			#if (i==5)
			#	{
			#	legend("center",legend=levels(new.var), col=(1:length(levels(new.var)))+1, pch=19, cex=0.5)
			#	}
			elec.new.var=setup[match(all.electrodes[i], electrodes),2]
			elec.color=match(elec.new.var, levels(setup$new.var))+1
			polygon(x,y, col=color.list[elec.color])
			}
		mtext(all.electrodes[i],side=3, line=-2)
		}
		for (k in 1:length(levels(elec.new.var)))
			{
			cat(levels(elec.new.var)[k], "-", color.list[k+1],"\n")
			}
	}
