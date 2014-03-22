#questa funzione si interfaccia con la funzione exportpicture_02_h, che esporta fil .eeg che contengono eeg.

import.erp=function(filenamebase,numbers, ext=".txt", outname="ERP_subj", envir=.GlobalEnv, fileinfo=FALSE){
	for (i in 1:length(numbers)){
		if (fileinfo==TRUE){
		eegout=read.table(paste(filenamebase, numbers[i], ext ,sep=""), header=T,skip=1)
		eeg.subjectname=readLines(paste(filenamebase, numbers[i], ext,sep=""), n=1)
		eeg.subjectname=gsub("\t","", eeg.subjectname)
		comment(eegout)=eeg.subjectname
		eegout.name=paste(outname, numbers[i], sep="")
		assign(eegout.name, eegout, envir=envir)
		}
		if (fileinfo==FALSE){
		eegout=read.table(paste(filenamebase, numbers[i], ext ,sep=""), header=T)
		eeg.subjectname=paste(filenamebase, numbers[i], ext ,sep="")
		eeg.subjectname=gsub("\t","", eeg.subjectname)
		comment(eegout)=eeg.subjectname
		eegout.name=paste(outname, numbers[i], sep="")
		assign(eegout.name, eegout, envir=envir)
		}
	}

}
