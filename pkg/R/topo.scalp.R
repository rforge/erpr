topo.scalp = function(erpdfs, smo=NULL, ylims="auto", yrev=TRUE, startmsec=NULL, endmsec=NULL, lwd=1, lty=1, 
                      color.list=c("blue", "red", "darkgreen"), legend=F, legend.lab="default", t.axis=NULL, 
                      elec.coord=NULL, el.toplot=NULL, x.margin=1, y.margin=1, ...)
{
  
  # retrive coordinates from topoplot (if not supplied)
  
  if (is.null(elec.coord)){
    coord=topoplot(return.coord=TRUE)
    coord$el.name=toupper(coord$el.name)
  } else {
    coord = elec.coord
  }
  
  ### check esistenza
  ### check esistenza
  if (is.data.frame(erpdfs)){
    categ = list(erpdfs)
  } else {
    categ = erpdfs
  }
  
  if (class(categ)!="list"){
    stop("input object must be a list of erpdfs or an erpdf!")
  }
  
  ### get startmsec from the first object
  erpdf = categ[[1]]
  
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
  
  
  # retrieve original electrode names
  # and change to upper to avoid inconsistencies with electrodes
  # with the elec.coord list from topoplot
  or.elnames = names(categ[[1]])
  for (k in 1:length(categ)){
    names(categ[[k]])=toupper(names(categ[[k]]))
  }
  
  
  
  # if nothing is specified plot all electrode
  if (is.null(el.toplot)){
    el.toplot=names(categ[[1]])
  }
  
  # retrive coordinates of electrode to plot
  curr.elec.coord=coord[toupper(coord$el.name)%in%toupper(el.toplot),]
  
  
  # creo le x e y con le coordinate.
  #if (projection=="equalarea"){
  x=curr.elec.coord$x *sqrt(1/(1 + curr.elec.coord$z))
  y=curr.elec.coord$y *sqrt(1/(1 + curr.elec.coord$z))
  
  ## convert original x from - 1 to 1, to 0 to 1
  # to uniform with fig=c(0,1,0,1)
  
  # NOTA: x.marin e per dare un po' di margine al plot (altrimenti 
  # gli elettrodi pi? esterni sarebbero troppo vicini al bordo
  
  # calcolo questi x2 che sono le coordinate degli elettrodi dopo 
  # 1) aver aggiunto dei margini
  #2) avere convertito le x e le y da -1, 1 a 0,1
  
  ###
  # VARIANDO X.MARGIN E Y.MARGIN, VARI LA GRANDEZZA DEL CERCHIO 
  # (QUINDI ELETTRODI PIU' O MENO VICINI)
  
  
  x1=(x-min(x-x.margin))
  x2=x1/max(x1+x.margin)
  
  y1=(y-min(y-y.margin))
  y2=y1/max(y1+y.margin)
  
  ########
  ## loop over categ (all objects to plot)
  #########
  for (k in 1:length(categ)){
    
    # draw an empty plot only at the first loop
    if (k==1){
    par(mar=c(0,0,0,0), fig=c(0,1,0,1))
    plot(0,0, type="n", frame.plot=F, xlab="", ylab="", axes=F)
    }
    
    #par(mar=c(0,0,0,0), fig=c(0,1,0,1))
    #plot(x2, y2, xlim=c(0-x.margin,1+x.margin), ylim=c(0-y.margin,1+y.margin))
    
    
    # x.size e y.size determinano la grandezza di ciascun pannello degli erp
    # determine size of each erp plot.
    x.size=0.1
    y.size=0.1
    
    
    ##########################
    #  PLOTTING CHECKS
    #i= 1
    # uncomment the follwoing line for some checks
    #polygon(x=c(x2[i]-(x.size/2), x2[i]-(x.size/2), x2[i]+(x.size/2), x2[i]+(x.size/2)), y=c(y2[i]-(y.size/2),y2[i]+(y.size/2), y2[i]+(y.size/2), y2[i]-(y.size/2)))
    
    # check di un elettrodo
    # points(x2[1], y2[1], cex=2)
    
    par(mar=c(0,0,0,0))
    
    
    # uso x2 come ciclo possibile
    for (i in 1:dim(curr.elec.coord)[1]){
      
      par(mar=c(0,0,0,0), new=TRUE, fig=c(x2[i]-(x.size/2), x2[i]+(x.size/2), y2[i]-(y.size/2), y2[i]+(y.size/2)))
      
      
      #plot(0.5, 0.5, frame.plot=F, xlim=c(0,1), ylim=c(0,1))
      #plot(word[, as.character(curr.elec.coord$el.name[i]) ], type="l", frame.plot=F, axes=F)
      erp(categ[[k]], as.character(curr.elec.coord$el.name[i]), draw.xaxis=F, draw.yaxis=F, startmsec=startmsec, endmsec=endmsec, col=color.list[k], frame.plot=F,...)
      
    }
  }
}

