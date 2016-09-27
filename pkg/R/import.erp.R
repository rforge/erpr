#questa funzione si interfaccia con la funzione exportpicture_02_h, che esporta fil .erp che contengono erp.

import.erp=function(filenamebase,numbers, ext=".txt", outname="ERP_subj", fileinfo=FALSE, erplist=NULL, path=getwd(), electrodes = "file", ...){
	
	outlist=list()
	length(outlist)=length(numbers)
	
	# check if the electrodes names are already in the file
	if (electrodes[1]=="file"){
	  myheader=TRUE # set the header to TRUE
	} else {
	  myheader=FALSE # set the header to FALSE
	}
	
	for (i in 1:length(numbers)){
		if (fileinfo==TRUE){
		erpout=read.table(paste(path, "/", filenamebase, numbers[i], ext ,sep=""), header=myheader,skip=1, ...)
		erp.subjectname=readLines(paste(filenamebase, numbers[i], ext,sep=""), n=1)
		erp.subjectname=gsub("\t","", erp.subjectname)
		comment(erpout)=erp.subjectname
		erpout.name=paste(outname, numbers[i], sep="")
		  if(myheader==FALSE){ # add electrodes names if specified
        names(erpout)=electrodes		  
		  }
		}
		if (fileinfo==FALSE){
		erpout=read.table(paste(path, "/", filenamebase, numbers[i], ext ,sep=""), header=myheader, ...)
		erp.subjectname=paste(filenamebase, numbers[i], ext ,sep="")
		erp.subjectname=gsub("\t","", erp.subjectname)
		comment(erpout)=erp.subjectname
		erpout.name=paste(outname, numbers[i], sep="")
		  if(myheader==FALSE){ # add electrode names if specified
		    names(erpout)=electrodes		  
		  }
		}
	outlist[[i]]=erpout
	names(outlist)[[i]]=erpout.name
	}
	if (!is.null(erplist)){
		outlist=c(erplist, outlist)
	}

return(outlist)
}
