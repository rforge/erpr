### CREATE ROI ####
# funzione per creare una colonna con ROI. 
# si utilizza con un dataset in formato lungo.

#datall = il dataset in formato lungo che contiene un vettore con i nomi degli elettrodi
# electrode = come testo, il nome della colonna con gli elettrodi.
# groups = lista che contiene (raggrupati in vettori separati), gli elettrodi da raggruppare. Es. groups=list(c("Fp1", "Fp2"), c("O1", "O2")), per
# 			creare due roi, una con Fp1 e Fp2, l'altra con O1 e O2.
#roi.levels= il nome che avranno i livelli della variabile ROI


# esempio di utilizzo.

#datall$caudality=create.roi(datall, electrode="electrode", groups=list(c("Fp1", "Fp2"), c("O1", "O2")), roi.levels=c("frontopolar", "ocular"))


create.roi=function(datall, electrode="electrode", groups=NULL, roi.levels=NULL ){
	if (!is.list(groups)){
		stop("groups must be a list!")
	}
	if (length(groups)!=length(roi.levels)){
		stop("groups and roi.levels must have the same length!")
	}
	
	roi=rep(NA, dim(datall)[1])
	for (i in 1:length(groups)){
		roi[datall[,electrode]%in%groups[[i]]]=roi.levels[i]
	}
	
	return(roi)
	
}
