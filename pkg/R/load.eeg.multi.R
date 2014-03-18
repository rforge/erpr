#questa funzione si interfaccia con la funzione exportpicture_02_h, che esporta fil .eeg che contengono eeg.

load.eeg.multi=function(filenamebase,numbers, outname="ERP_subj", env=.GlobalEnv){
	for (i in 1:length(numbers)){
		eegout=read.table(paste(filenamebase, numbers[i],".txt",sep=""), header=T,skip=1)
		eeg.subjectname=readLines(paste(filenamebase, numbers[i], ".txt",sep=""), n=1)
		eeg.subjectname=gsub("\t","", eeg.subjectname)
		comment(eegout)=eeg.subjectname
		eegout.name=paste(outname, numbers[i], sep="")
		assign(eegout.name, eegout, envir=env)
		}
}
