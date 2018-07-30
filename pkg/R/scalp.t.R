scalp.t <-
  function(base1, base2, numbers1, numbers2=NULL, paired=TRUE, alpha=0.05, sig=NULL, erplist1=NULL, erplist2=erplist1, smo=NULL, layout=1, ylims="auto", yrev=TRUE, startmsec=NULL, endmsec=NULL, lwd=c(1,1), lty=c(1,1), color.list=c("blue", "red"), legend=F, legend.lab="default", t.axis=NULL, scalp.array=NULL, p.adjust.method="none") {
    
    
    # preliminary checks
    if (is.null(erplist1)|is.null(erplist2)){
      stop("two erplist objects (erplist1 and erplist2) containing ERP data frames must be specified!", call.=F)
    }
    
    # consistency checks for paired =T
    if (paired==TRUE&(length(numbers1)!=length(numbers2))){
      stop ("if paired == TRUE, numbers1 and numbers2 must have equal length.", call.=F)
    }
    
    #### object checks
    object.names1=paste(base1, numbers1, sep="")
    if (any(!object.names1%in%names(erplist1))){
      missing.objects1=object.names1[!object.names1%in%names(erplist1)]
      missing.object.collist1=paste(missing.objects1, "\n", sep="")
      stop("The following objects are not contained in the erplist1 specified:\n", missing.object.collist1, call.=F)
    }
    #### object checks
    object.names2=paste(base2, numbers2, sep="")
    if (any(!object.names2%in%names(erplist2))){
      missing.objects2=object.names2[!object.names2%in%names(erplist2)]
      missing.object.collist2=paste(missing.objects2, "\n", sep="")
      stop("The following objects are not contained in the erplist2 specified:\n", missing.object.collist2, call.=F)
    }
    
    ### get startmsec from the first object
    erplist=erplist1
    erpdf = erplist[[1]]
    
    if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
      startmsec=attr(erpdf, "startmsec")
      endmsec=attr(erpdf, "endmsec")
    }
    
    if (is.null(startmsec)|is.null(endmsec)){
      stop("startmsec and endmsec must be specified", call.=F)
    }
    
    # create t.axis if not supplied (step by 200)
    if (is.null(t.axis)){
      t.axis=seq(startmsec, endmsec, 200)
    }
    
  
    
    if (length(legend.lab)==1&legend.lab[1]=="default"){
      legend.lab=c(base1, base2)
    }
    
  
    
    ### FUNZIONE PER FARE AVERAGE PER PLOT
    
    #base1 = le prime lettere degli oggetti 
    #numbers1= il numero dei soggetti di cui calcolare l'average
    
    alldata1=grandaverage(base=base1, numbers1, erplist=erplist1)
    alldata2=grandaverage(base=base2,numbers2, erplist=erplist2)
    
    categ=list(alldata1,alldata2)
    
    if (class(categ)!="list"){
      stop("input object must be a list!!")}
    
    
    if (layout[1]==1){
      electrodes=c("yaxis","Fp1", "blank", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T3", "C3", "CZ","C4","T4","TP7", "CP3", "CPZ", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "xaxis", "O1", "OZ", "O2", "blank")
    }
    if (layout[1]==2){
      electrodes=c("yaxis","Fp1", "FPZ", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T7", "C3", "CZ","C4","T8","TP7", "CP3", "CPZ", "CP4", "TP8", "P7", "P3", "PZ", "P4", "P8", "xaxis", "O1", "OZ", "O2", "blank")
    }
    if (layout[1]==3){
      electrodes=c("yaxis","Fp1", "Fpz", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCz", "FC4", "FT8", "T3", "C3", "Cz","C4","T4","TP7", "CP3", "CPz", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "xaxis", "O1", "blank", "O2", "blank")
    }
    if (layout[1]==4){
      electrodes=c("yaxis", "Fp1", "blank", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "FCz", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "blank", "CP1", "CPz", "CP2", "blank", "P7", "P3", "Pz", "P4", "P8", "blank","O1","blank", "O2", "blank")
    }
    if (layout[1]==5){
      electrodes=c("yaxis", "Fp1", "Fpz", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "blank", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "CP5", "CP1", "blank", "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8","PO7", "PO3", "POz", "PO4", "PO8", "blank","O1","Oz", "O2", "blank" )
    }
    if (length(layout)>1){
      electrodes=layout
    }		
    
    
    ## ci sono incongruenze con le etichette degli elettrodi. Per non fermarmi le cambio momentaneamente nella seguente #maniera T7=T3, T4=T8, P7=T5, T6=P8
    
    
    #### PARTE 1: STATISTICHE PER ELETTRODO ####
    if (is.null(numbers2)){
      numbers2=numbers1}
    
    
    if (is.null(sig)){
      
      electrodestoanalyze=electrodes[electrodes%in%names(alldata1)]
      
      alltemp.results=mass.t.test(base1=base1, base2=base2, numbers1=numbers1, numbers2=numbers2, startmsec=startmsec, endmsec=endmsec, paired=paired, erplist1
                                  =erplist1, erplist2=erplist2, electrodes=electrodestoanalyze, p.adjust.method=p.adjust.method)$sig
      
      
    }
    if (!is.null(sig)){
      alltemp.results=sig
    }
    
    
    
    ##### PARTE 2 CREO DATAFRAME PER SCALP
    
    
    
    if (ylims=="auto"){
      ## mergio tutti i dataset per riscalare gli assi rispetto a massimo e minimo 
      alldata=NULL
      for (i in 1:length(categ)){
        alldata=rbind(alldata, categ[[i]])
      }
      ymax=max(alldata)
      ymin=min(alldata)
      yedge=max(c(ymax, abs(ymin)))#calcolo questo yedge in modo da fare limiti delle y simmetrici
      # aggiungo una perecentuale per evitare che il grafico sbordi (il)
      yedge=c(-yedge,yedge)
    }
    if (ylims!="auto"){
      yedge=ylims
      yedge=c(-ylims, ylims)
    }	
    
    if (yrev==TRUE){
      yedge=sort(yedge, decreasing=T)
    }
    
    oldpar <- par(no.readonly=TRUE) #This is to reset old parameter. Taken from "An introduction to R" pag. 68. Maybe deprecated.
    
    par(mfrow=c(7,5), mai=c(0,0,0,0))
    
    if (layout[1]==5)
    {
      par(mfrow=c(10,5), mai=c(0,0,0,0))
    }
    if (layout[1]==4)
    {
      par(mfrow=c(8,5), mai=c(0,0,0,0))
    }
    if (!is.null(scalp.array)){
      par(mfrow=scalp.array, mai=c(0,0,0,0), oma=c(0,2,0,2))
    }
    
    
    for (i in 1:(length(electrodes))){
      if (electrodes[i]=="yaxis"){
        plot(1, type="n", frame.plot=FALSE,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))
        axis(side=2, pos= dim(categ[[1]])[1]/2, at=c(round(ceiling(yedge[1]),0),round(ceiling(yedge[1])/2,0),0,round(floor(yedge[2])/2,0),round(floor(yedge[2]),0)), cex.axis=0.8, las=2)
        text((dim(categ[[1]])[1]/2)+(dim(categ[[1]])[1]/8),0, labels=expression(paste(mu,"V")), cex=1.4)
      }
      if (electrodes[i]=="blank") {
        plot.new()
      }
      if (electrodes[i]=="legend"){
        plot.new()
        if (legend=="TRUE"){
          legend("center", legend=legend.lab, col=color.list, cex=1.2, lty=lty, lwd=lwd) #pch=15, pt.bg=color.list
        }
      }
      if (electrodes[i]=="xaxis"){
        plot(1, type="n", frame.plot=FALSE,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))
        
        axis(1, pos=0, at=msectopoints(t.axis, dim(categ[[1]])[1], startmsec, endmsec), labels=paste(t.axis))
      }
      if (!electrodes[i]%in%c("xaxis", "yaxis", "legend", "blank")) {
        
        ### NOTA: plotto due volte il grafico: la prima volta con type="n" poi con type="l". Altrimenti le bande si sovrascrivono col grafico
        el=categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]]
        
        
        plot(el, type="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)),col=color.list[1], main="", ylab="", xlab="", cex.main=0.85,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n",frame.plot=FALSE, lwd=lwd[1], lty=lty[1])
        
        # plot significance bands.
        ######################
        abline(v=grep(TRUE,alltemp.results[,electrodes[i]]), col="lightgray")
        #######################
        
        if (!is.null(smo)){
          el=smooth.spline(el, spar=smo)
        }
        
        el=categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]]
        
        ### NOTA: plotto due volte il grafico: la prima volta con type="n" poi con type="l". Altrimenti le bande si sovrascrivono col grafico
        lines(el, col=color.list[1],  cex.main=0.85, lwd=lwd[1], lty=lty[1])
        
        ##### di seguito ho semplicemente calcolato, tramite una proporzione, il punto che corrisponde allo 0
        zeropoint=msectopoints(0, length(el), startmsec, endmsec)
        segments(x0=zeropoint, y0=-0.8, x1=zeropoint, y1=0.5, lwd=1.5)
        
        
        abline(h=0, lty="longdash")
        mtext(electrodes[i],side=3, line=-2)
        
        el2=categ[[2]][[electrodes[i]]]
        if (!is.null(smo)){
          el2=smooth.spline(el2, spar=smo)
        }						
        lines(el2,col=color.list[2], lwd=lwd[2],lty=lty[2])
        
        
        
      }
    }
    par(oldpar)# to reset old parameters
    invisible(alltemp.results)
  }
