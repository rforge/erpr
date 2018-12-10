import.erp=function(filenamebase, numbers, ext=".txt", outname="ERP_subj", fileinfo=FALSE, abstimeinfo=FALSE, erplist=NULL, path=getwd(), electrodes = "file", startmsec=NULL, endmsec=NULL, values.multi=1, ...){
  
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
      erpout = erpout*values.multi
      erp.subjectname=readLines(paste(path, "/", filenamebase, numbers[i], ext,sep=""), n=1)
      erp.subjectname=gsub("\t","", erp.subjectname)
      comment(erpout)=erp.subjectname
      
      # add absolute time info if present
      # the time info is retrieved after this string "Time = " (the space is not important)
      if (abstimeinfo==TRUE){
        temp = strsplit(erp.subjectname, "Time = ")[[1]][[2]] # retrieve what's after time
        Time = as.numeric(strsplit(temp, ";")[[1]][[1]]) # retrieve what's before the next ;
        attr(erpout, "TrialTime")=Time
      }
      
      erpout.name=paste(outname, numbers[i], sep="")
      if(myheader==FALSE){ # add electrodes names if specified
        names(erpout)=electrodes		  
      }
    }
    if (fileinfo==FALSE){
      erpout=read.table(paste(path, "/", filenamebase, numbers[i], ext ,sep=""), header=myheader, ...)
      erpout = erpout*values.multi
      erp.subjectname=paste(path, "/", filenamebase, numbers[i], ext ,sep="")
      erp.subjectname=gsub("\t","", erp.subjectname)
      comment(erpout)=erp.subjectname
      erpout.name=paste(outname, numbers[i], sep="")
      if(myheader==FALSE){ # add electrode names if specified
        names(erpout)=electrodes		  
      }
    }
    
    if(!is.null(startmsec)){
      attr(erpout, "startmsec")=startmsec
    }
    
    if(!is.null(endmsec)){
      attr(erpout, "endmsec")=endmsec
    }
    
    outlist[[i]]=erpout
    names(outlist)[[i]]=erpout.name
  }
  if (!is.null(erplist)){
    outlist=c(erplist, outlist)
  }
  
  return(outlist)
}
