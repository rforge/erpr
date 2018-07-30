mass.t.test<-function(base1=NULL, base2=NULL, numbers1=NULL, numbers2=numbers1, startmsec=NULL, endmsec=NULL, paired=F, erplist1=NULL, erplist2=erplist1,  electrodes="all", to.exclude=NULL, interval=c(startmsec, endmsec), p.adjust.method="none", n.permutations=1000, p.crit=0.05, 
                      stable.diff=FALSE, crit.msec=NULL, crit.npoints=NULL, neighbours=NULL, min_nchans=2, na.rm=TRUE) {
  
  
  # NOTE: this is mass.t.test version 7
  # cluster based permutation implemented
  
  
  # results include:
  # - t.res.mat: the full matrix with T-values: NA are added in the timepoitns not analized.
  # - or.res.filt.mat: the matrix, dysplaing only significant t-values
  # - or.res.log.mat: a logical matrix. TRUE where p values were < p.crit (after corrections)
  # - or.res.p.mat: matrix with observed p-values.
  
  # all this matrices have the same timepoints as the ORIGINAL supplied matrices.
  
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
  
  
  
  ###
  # STEP 1 LOAD DATA
  #
  
  # retrieve names from first subject.
  if (electrodes[1]=="all"){
    electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])
  }
  
  ### to.exclude overwrite electrodes!
  if (!is.null(to.exclude)){
    electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])[!names(erplist1[[paste(base1, numbers1[1], sep = "")]])%in%to.exclude]
  }
  
  ### select interval to be analyzed.
  startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
  endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[2], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
  
  or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
  new.dim1=length(startpoint:endpoint)
  
  # calculate exact interval analyzed
  # the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
  # the exact values considered will be returned.
  exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
  exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
  
  ###################
  # CREATE DATALL ###
  ###################
  # datall is a matrix with subject in columns and time x electrodes in rows.
  # if an interval is specified, datall already select the correct timepoints.
  
  # store these info for future use
  n_timepoints = length(startpoint:endpoint)
  n_electrodes = length(electrodes)
  
  datall1 = NULL
  for (i in 1:length(numbers1)) {
    x.temp = erplist1[[paste(base1, numbers1[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
    
    # add exception if only one electrode is included
    if(is.null(dim(x.temp)[1])){
      x.temp=data.frame(x.temp)
      names(x.temp)=electrodes
    }
    
    x.vec=unlist(x.temp)
    if (i == 1) {
      datall1 = x.vec
    }
    if (i != 1) {
      datall1=cbind(datall1, x.vec)
    }
  }
  
  datall2 = NULL
  for (i in 1:length(numbers2)) {
    x.temp = erplist2[[paste(base2, numbers2[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
    
    # add exception if only one electrode is included
    if(is.null(dim(x.temp)[1])){
      x.temp=data.frame(x.temp)
      names(x.temp)=electrodes
    }
    
    
    x.vec=unlist(x.temp)
    if (i == 1) {
      datall2 = x.vec
    }
    if (i != 1) {
      datall2=cbind(datall2, x.vec)
    }
  }
  
  #########################################
  # T.TESTS POINT by POINT (calculate, by dfaults, all t-tests)
  #########################################
  
  
  # initialize some objects
  allres=rep(NA, dim(datall1)[1]) # uso datall1 come riferimento ma la dimensione (i timepoints) saranno uguale a datall2
  allres.t=rep(NA, dim(datall1)[1]) # creo matrice per valori di t
  
  
  ### OBSERVED T-TESTS: BETWEEN
  if (paired ==  FALSE) {
    
    
    for (i in 1:dim(datall1)[1]){
      
      # res=t.test(datall1[i,], datall2[i, ], var.equal=T, paired=F)$p.value
      
      df=length(datall1[i, ]) -1 + length(datall2[i, ]) -1  # remember that cols of datall are the subjects.
      t.crit=qt(p.crit/2, df=df) # non lo uso per ora. Non sono manco sicuro sia giusto
      
      res.t=(mean(datall1[i, ], na.rm=T)-mean(datall2[i, ], na.rm=T))/sqrt(var(datall1[i,], na.rm=T)/length(datall1[i,])+var(datall2[i, ], na.rm=T)/length(datall2[i,])) # independent samples.
      
      res=pt(abs(res.t), df=df, lower.tail=F)*2
      
      allres[i]=res #p-value
      
      allres.t[i]=res.t
      
    }
  }
  
  ### OBSERVED T-TESTS: PAIRED
  if (paired == TRUE) {
    
    #df=length(datall1[i, ]) -1 
    
    for (i in 1:dim(datall1)[1]){
      
      df= sum(!is.na(datall1[i, ])) - 1 
      t.crit=qt(p.crit/2, df=df) # non lo uso per ora.
      
      res.t=mean((datall1[i, ]-datall2[i, ]), na.rm = na.rm)/sqrt((var(datall1[i, ]-datall2[i, ], na.rm=na.rm)/length(datall1[i, ])))
      # dependent samples
      
      res=pt(abs(res.t), df=df, lower.tail=F)*2
      
      
      allres[i]=res
      
      
      allres.t[i]=res.t
      
    }
  }
  
  ########################################
  #### APPLY P.ADJUST CORRECTIONS ########
  #######################################
  # apply corrections already implemented in 
  # p.adjust.methods function
  
  if (!p.adjust.method%in%c("tmax", "cluster.based.permutation")){
    
    allres=p.adjust(allres, method=p.adjust.method)
  }
  
  
  ########################################
  #### tMAX permutations ########
  #######################################
  if (p.adjust.method=="tmax"){
    
    # permutations are made from datall1 and datall2
    
    if (paired == FALSE){ # tmax for unpaired case
      
      # all res t was already calculated  
      allres.t.obs=allres.t
      
      # set timer for permutations:
      n.points.time=floor(seq(1, n.permutations, n.permutations/10))
      time.elapsed=0
      
      # set length outside the for loop to speed up computation.
      # I will need them inside the loop for i in 1:n.permutations
      datall1.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
      datall2.perm.len=rep(dim(datall2)[2], dim(datall2)[1])
      
      cat("computing permutations:\n")
      
      for (i in 1:n.permutations){
        
        perm.index1=sample(all.numbers, length(numbers1))
        datall1.perm=datall12[,perm.index1]
        datall2.perm=datall12[,-perm.index1]
        
        curr.perm.t=rep(NA, dim(datall1.perm)[1])
        
        
        
        
        datall1.perm.mean=rowMeans(datall1.perm)
        datall2.perm.mean=rowMeans(datall2.perm)
        datall1.perm.var=rowSums((datall1.perm-datall1.perm.mean)^2)/(datall1.perm.len)
        datall2.perm.var=rowSums((datall2.perm-datall2.perm.mean)^2)/(datall2.perm.len)
        
        
        curr.perm.t=(datall1.perm.mean-datall2.perm.mean)/sqrt(datall1.perm.var/datall1.perm.len+datall2.perm.var/datall2.perm.len)
        
        
        
        if (i%in%n.points.time){
          cat(rep(".",10-time.elapsed), "\n")
          time.elapsed=time.elapsed+1
        }
        
        t.max.res[i]=max(abs(curr.perm.t))
        
      }
      
      p.corr=sapply(allres.t.obs, function(x){(sum(as.numeric(t.max.res>=abs(x)))/length(t.max.res))})
      allres=p.corr
      allres.t=allres.t.obs
      
    } # end if method = tmax paired =F	
    
    
    if (paired == TRUE) {# t max paired case
      
      all.res.t.obs = 
        
        # create object with all t.max
        t.max.res=rep(NA, n.permutations)
      
      # create object with length to speed-up computation
      datall.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
      
      
      # set timer for permutations:
      n.points.time=floor(seq(1, n.permutations, n.permutations/10))
      time.elapsed=0
      cat("computing permutations:\n")
      
      for (i in 1:n.permutations){
        
        
        perm=sample(c(1,-1), length(numbers1), replace=T)
        
        datall.perm=t(t(datall12)*perm)
        
        datall.perm.mean=rowMeans(datall.perm)
        datall.perm.var=rowSums((datall.perm-datall.perm.mean)^2)/(datall.perm.len)
        
        curr.perm.t=datall.perm.mean/sqrt(datall.perm.var/datall.perm.len)
        
        
        t.max.res[i]=max(abs(curr.perm.t))
        
        if (i%in%n.points.time){
          cat(rep(".",10-time.elapsed), "\n")
          time.elapsed=time.elapsed+1
        }	
      }
      
      p.corr=sapply(allres.t.obs, function(x){(sum(as.numeric(t.max.res>=abs(x)))/length(t.max.res))})
      allres=p.corr
      allres.t=allres.t.obs
    } # end if method = tmax, paired = T
    
  } # end if method = tmax
  
  
  ##############################
  # reconstruct the matrix to return the reuslts
  ##############################
  # the following part is the same for simple t.tests and tmax permutation
  # I write it here to avoid replicating code.
  # the results may also be fed to the stable.diff fun.
  # NOTE: that the following code cannot be used for cluster based permutation
  # in which negative and positive results are filtered separately
  
  ## filtro i risultati  di res.t.mat a partire da crit.mat e riarrangio al volo.
  res.mat=matrix(allres, nrow=n_timepoints, ncol=n_electrodes, byrow=F)
  res.t.mat=matrix(allres.t, nrow=n_timepoints, ncol=n_electrodes, byrow=F)
  res.p.mat=matrix(allres, nrow=n_timepoints, ncol=n_electrodes,  byrow=F)
  colnames(res.t.mat)=electrodes
  
  
  #### 
  
  res.mat.filter=matrix(NA, nrow=dim(res.mat)[1], ncol=dim(res.mat)[2])
  res.log.mat=res.mat<p.crit
  res.mat.filter[res.log.mat]=TRUE
  
  filt.mat=matrix(res.t.mat[res.mat.filter], nrow=nrow(res.mat), ncol=ncol(res.mat))
  
  ## reconstruct a data.frame with the original number of time points before returning results
  # "or" prefix indicates that the dimensions are referred to the original data dimensions
  or.filt.mat=as.data.frame(matrix(rep(NA, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2]))
  
  # IMPORTANT! here the matrix is compsed by FALSE or TRUE (no NA) to be used properly with stable.diff
  or.res.log.mat=as.data.frame(matrix(rep(FALSE, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2])) # creo lo stesso per risultati log
  or.res.t.mat=or.filt.mat # the same for t.
  or.res.p.mat=or.filt.mat # and for p
  
  or.filt.mat[startpoint:endpoint, ]=filt.mat
  or.res.log.mat[startpoint:endpoint, ]=res.log.mat
  or.res.t.mat[startpoint:endpoint, ]=res.t.mat
  or.res.p.mat[startpoint:endpoint, ]=res.p.mat
  
  
  colnames(or.filt.mat)=names(x.temp)
  colnames(or.res.log.mat)=names(x.temp)
  colnames(or.res.t.mat)=names(x.temp)
  colnames(or.res.p.mat)=names(x.temp)
  
  #########################################
  ####### CALCULATE EXACT MS OF INTERVAL
  #########################################
  startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
  endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
  
  or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
  new.dim1=length(startpoint:endpoint)
  
  # calculate exact interval analyzed
  # the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
  # the exact values considered will be returned.
  exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
  exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
  
  ############################################
  # STABLE DIFF (applied to regular t-tests and t-max)
  ############################################
  if (stable.diff==TRUE){ 
    
    ####### CALCULATE CRIT.MSEC (for stable.diff)
    
    mysamp.rate=sampling.rate(erplist1[[paste(base1, numbers1[1], sep = "")]], startmsec=startmsec, endmsec=endmsec)
    
    if (!is.null(crit.msec)){
      crit.npoints=round(crit.msec/(1000/mysamp.rate))
    }
    
    # calculate the exact ms, to be returned in param
    exact.crit.msec=crit.npoints*(1000/mysamp.rate)
    
    stable.diff.res=stable.diff.fun(or.res.log.mat, electrodes, crit.npoints, interval, startmsec, endmsec)
    
    # "filter" again the data, on the basis of stable.diff.fun results
    
    #print(str(stable.diff.res, 1))
    or.filt.mat[!stable.diff.res$res.log.mat]=NA
    or.res.log.mat[!stable.diff.res$res.log.mat]=NA
    or.res.t.mat[!stable.diff.res$res.log.mat]=NA
    or.res.p.mat[!stable.diff.res$res.log.mat]=NA
    
  } # end if (stable.diff==TRUE)
  
  #######################################
  #### CLUSTER BASED PERMUTATION
  #######################################
  
  if (p.adjust.method=="cluster.based.permutation"){
    
    ##################################
    # CREATE AN ADDITIONAL FUNCTION (it will be used to calculate cluster mass)
    ##################################
    
    # and makes the code cleaner
    cluster_mass <-function(clusters, tmat){
      # IMPORTANT: the function assumes that clusters
      # is a electrodes*timepoints matrices (as the results of find.clusters)
      # and tmat is a timepoints*electrodes
      
      if(!is.null(clusters)){
        clusters.vec = unlist(t(clusters))
        tmat.vec = unlist(tmat)
        clust_mass.vec = tapply(tmat.vec, clusters.vec, sum)
        # remove cluster 0. No clusters is treated with 0 by the find.clusters function. 
        clust_mass.vec = clust_mass.vec[names(clust_mass.vec)!="0"]
        return(as.numeric(clust_mass.vec))
      } else {
        # if there are no clusters a 0 is returned (like no mass)
        # Not sure this is correct, but if NA is resturned, than no cluster found
        # during permutation is excluded
        return(0)
      }
    }
    
    
    #######################################
    #### CLUSTER BASED PERMUTATION
    #######################################
    
    # initilaize neighbours
    neighbours = NULL
    
    ## check if it possible to perform the cluster based the cluster based permutation
    
    
    ### WITH THIS FUNCTION I MUST RETURN
    # - significant results filtered  (perm.or.filt.result)
    # - a logical matrix with significance: check the format in mass.t. here 
    #        temp.res.log.mat is maybe NOT ok, cause it has NA inside. Check
    # - cluster membership
    # - cluster exact p.values
    
    
    # preliminary check if there is any significant result (otherwise the functions called
    # for the cluster based permutation will give errors.)
    if(all(is.na(or.filt.mat))){
      
      stop("there are no significant results and cluster based permutation cannot be performed")
      
    }
    
    
    # steps of cluster based permutation.
    # taken from Groppe, Urbach, & Kutas (2011). Mass univeriate analysis of event-related brain potential/fields 1: A critical tutorial review.
    
    # step 1) calculate t.scores.  
    # step 2) exclude p not exceding a threshold (e.g. p>= 0.05)
    # step 3a) determine neighbours (added by G)
    # step 3) optional (use only t scores with at least some significant neighbours)
    # step 4) on remaining t.scores, form cluster (the permutation start here!)
    # step 5) sum values of each cluster and find cluster.level.t. this is the cluster level t.score
    # step 6) On each permutation get the more extreme values to define the null hypothesis ditsribution(as in tmax) (the permutation end here!)
    # step 7) p.value of each cluster is derived from its ranking on the null hypothesis distribution
    # step 8) each member of the cluster is assigned the p-value of the entire cluster.
    
    
    # step 5) A) determine neighbours if not supplied
    # if you don't specify
    if(is.null(neighbours)){
      
      ## retrieve electrode coordinates data.frame from topoplot
      elec.coord=topoplot(return.coord=TRUE)
      
      # electrode names can be different simply for having upper-case or lower-case letters
      # tackle this issue by making all electrodes upper-case
      
      # transform current electrode names toupper to match them with the electrode list from topoplot
      up.curr.el = toupper(electrodes)
      # transform all electrode names in list to upper
      up.elec.coord.names = toupper(elec.coord$el.name)
      
      # retrieve current electrode coordinates
      curr.elec.coord = elec.coord[up.elec.coord.names %in% up.curr.el, ]
      
      # return a matrix n_electrodes x n_electrodes with 0/1 indicating neigbourhood
      neighbours=spatial_neighbours(curr.elec.coord[, c("x", "y", "z")], 1)
    }
    
    ## step 5) FIND CLUSTERS
    
    # the find cluster from Groppe uses a threshold (positive or negative).
    # you have already calculated this critical t in the first mass univariate
    # the object is called "t.crit" (see inside the mass t.test calculations) 
    
    # NOTE: you need to calculate two times the cluster, one for positive, and one for negative
    # cluster. THis is because clusters are calculated according to sign.
    # (Note that I transform the threshold to positive to be sure to know its sign, positive or negative)
    # in the call to find.cluster
    
    
    # first I calcualte the actual clusters of my data, to be compared with the empirical distributions
    # of cluster masses.
    
    perm.thresh=abs(t.crit)
    
    ## the function find.cluster does not work with NA.
    # filt.mat (the data I need to work with, is full of NA)
    # hence I substitute NA with 0 (that I know for sure they won't be taken as significant value)
    
    # store a new object called "obs.perm.mat", to keep the original "filt.mat" object
    obs.perm.filt.mat=filt.mat
    
    obs.perm.filt.mat[is.na(obs.perm.filt.mat)]=0
    
    #######################################
    # 1) CALCULATE ACTUAL CLUSTERS
    ########################################
    
    
    # find positive clusters
    obs.posclust=find.clusters(t(obs.perm.filt.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1)
    # find negative clusters
    obs.negclust=find.clusters(t(obs.perm.filt.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1)
    
    # NOTE!! the current find.clusters function return either a 
    # electrodes*timepoints matrix OR just the NULL.
    # you should modify the function to avoid exceptions in the following code.
    
    ##########################################
    ## Calculate MASS of CLUSTERS
    ###########################################  
    # the mass of a cluster is the sum of t-values of all members of a cluster
    
    
    ##########################################
    ## Calculate mass of positive clusters 
    ###########################################
    mass.obs.posclust=cluster_mass(obs.posclust, obs.perm.filt.mat)
    ##########################################
    ## Calculate mass of negative clusters 
    ###########################################
    mass.obs.negclust=cluster_mass(obs.negclust, obs.perm.filt.mat)
    
    
    
    # permutations are made from datall1 and datall2
    
    if (paired == FALSE){
      
      # create a unique data.frame with all subjects
      # the aim is to permute every subject swiching subject
      # group in the permutations
      # notice that this is not necessary with paired=T
      # in that case I simply permute the signs of the 
      # condition differences
      
      
      datall12=cbind(datall1, datall2)
      # creo vettore con numero totale di soggetti
      all.numbers=1:dim(datall12)[2] 
      
      
      # create object with all permutations results
      # the function ha two rows: the first for 
      # positive cluster max results and the second
      # for negative cluster max results.
      # This is because the p-values of positive and negative clusters 
      # are calculated separately
      # 
      clust.perm.max.res=matrix(0, nrow=2, ncol=n.permutations)
      
      
      # set length outside the for loop to speed up computation.
      # I will need them inside the loop for i in 1:n.permutations
      datall1.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
      datall2.perm.len=rep(dim(datall2)[2], dim(datall2)[1])
      
      
      # set timer for permutations:
      n.points.time=floor(seq(1, n.permutations, n.permutations/10))
      time.elapsed=0
      
      
      
      #### START PERMUTATIONS HERE
      cat("computing permutations:\n")
      
      for (i in 1:n.permutations){
        
        perm.index1=sample(all.numbers, length(numbers1))
        datall1.perm=datall12[,perm.index1]
        datall2.perm=datall12[,-perm.index1]
        
        curr.perm.t=rep(NA, dim(datall1.perm)[1])
        
        
        datall1.perm.mean=rowMeans(datall1.perm)
        datall2.perm.mean=rowMeans(datall2.perm)
        datall1.perm.var=rowSums((datall1.perm-datall1.perm.mean)^2)/(datall1.perm.len)
        datall2.perm.var=rowSums((datall2.perm-datall2.perm.mean)^2)/(datall2.perm.len)
        
        
        curr.perm.t=(datall1.perm.mean-datall2.perm.mean)/sqrt(datall1.perm.var/datall1.perm.len+datall2.perm.var/datall2.perm.len)
        
        ## reconstruct the data, to use the various cluster perm functions (which require matrices)
        curr.perm.t.mat=matrix(curr.perm.t, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(  obs.perm.filt.mat)[2], byrow=F)
        colnames(curr.perm.t.mat)=colnames(obs.perm.filt.mat)
        
        #######################################
        # 1) CALCULATE CURRENT PERMUTATION CLUSTERS
        ########################################
        
        # NOTICE that curr.perm.t.mat is filtered both for signficance
        
        
        # find positive clusters
        curr.posclust=find.clusters(t(curr.perm.t.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1)
        # find negative clusters
        curr.negclust=find.clusters(t(curr.perm.t.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1)
        
        ##########################################
        ## Calculate MASS of CLUSTERS (within perm)
        ###########################################  
        
        
        ##########################################
        ## Calculate mass of positive clusters 
        ###########################################
        mass.curr.posclust=cluster_mass(curr.posclust, curr.perm.t.mat)
        ##########################################
        ## Calculate mass of negative clusters 
        ###########################################
        mass.curr.negclust=cluster_mass(curr.negclust, curr.perm.t.mat)
        
        
        # 3) then store the maximum mass cluster value in the object clust.perm.max.res
        # note that I first concatenate all values, then transform in absolute value, and
        # then I find the max.
        
        ## ERRORE GIORGIO
        # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/clust_perm2.m
        # qui è segnato che i p-value di negative cluster e positive devono essere diversi.
        # -When doing two-tailed tests it is possible to get p-values greater than 1.
        # These can be treated equivalent to a p-value of 1. The reason for
        # this is that the p-values for positive and negative clusters are computed
        # from two different distributions of permuted values.  For a non-cluster
        # based permutation test, there is only one distribution of permuted
        # values.
        
        
        ## store the results of max and min cluster separately
        # (NOTA GIORGIO: controlla il codice nella funzione di Groppe per capire esattamente questa parte.
        clust.perm.max.res[1, i]=max(c(mass.curr.posclust))
        clust.perm.max.res[2, i]=min(c(mass.curr.negclust))
        
        
        if (i%in%n.points.time){
          cat(rep(".",10-time.elapsed), "\n")
          time.elapsed=time.elapsed+1
          
        }
        
      } # end for i in 1:permutations in clust based perm and paired = F
      
      
    } # end if method = clust based perm & paired =F	
    
    
    # GIORGIO UNCOMMENT
    if (paired == TRUE) { # cluster.based.permutation paired case
      
      
      # datall12 is the dataframe with the difference of datall1 and datall2.
      datall12=datall1-datall2
      
      # create object with all permutations results
      # the function ha two rows: the first for 
      # positive cluster max results and the second
      # for negative cluster max results.
      # This is because the p-values of positive and negative clusters 
      # are calculated separately
      # 
      clust.perm.max.res=matrix(0, nrow=2, ncol=n.permutations)
      
      # create object with length to speed-up computation
      # the object nsubjects * (points*nelectrodes)
      datall.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
      
      
      # set timer for permutations:
      n.points.time=floor(seq(1, n.permutations, n.permutations/10))
      time.elapsed=0
      cat("computing permutations:\n")
      
      # GIORGIO UNCOMMENT
      
      for (i in 1:n.permutations){
        
        # assign -1 and 1 to switch labels for the permutations
        perm=sample(c(1,-1), length(numbers1), replace=T)
        
        # multiply the matrix with observed t.
        # the effect is to "switch" labels of the condition (since datall12 is already the difference A-B)
        datall.perm=t(t(datall12)*perm)
        
        datall.perm.mean=rowMeans(datall.perm)
        datall.perm.var=rowSums((datall.perm-datall.perm.mean)^2)/(datall.perm.len)
        
        curr.perm.t=datall.perm.mean/sqrt(datall.perm.var/datall.perm.len)
        
        # reconstruct the data, in order to use find.clusters and stable.diff.fun
        curr.perm.t.mat=matrix(curr.perm.t, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(obs.perm.filt.mat)[2], byrow=F)
        colnames(curr.perm.t.mat)=colnames(obs.perm.filt.mat)
        
        
        #######################################
        # 1) CALCULATE CURRENT PERMUTATION CLUSTERS
        ########################################
        
        # NOTICE that curr.perm.t.mat is filtered both for signficance
        
        
        # find positive clusters
        curr.posclust=find.clusters(t(curr.perm.t.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1)
        # find negative clusters
        curr.negclust=find.clusters(t(curr.perm.t.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1)
        
        ##########################################
        ## Calculate MASS of CLUSTERS
        ###########################################  
        
        ##########################################
        ## Calculate mass of positive clusters 
        ###########################################
        mass.curr.posclust=cluster_mass(curr.posclust, curr.perm.t.mat)
        ##########################################
        ## Calculate mass of negative clusters 
        ###########################################
        mass.curr.negclust=cluster_mass(curr.negclust, curr.perm.t.mat)
        
        
        # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/clust_perm2.m
        # -When doing two-tailed tests it is possible to get p-values greater than 1.
        # These can be treated equivalent to a p-value of 1. The reason for
        # this is that the p-values for positive and negative clusters are computed
        # from two different distributions of permuted values.  For a non-cluster
        # based permutation test, there is only one distribution of permuted
        # values.
        
        ## store the results of max and min cluster separately
        clust.perm.max.res[1, i]=max(c(mass.curr.posclust))
        clust.perm.max.res[2, i]=min(c(mass.curr.negclust))
        
        
        if (i%in%n.points.time){
          cat(rep(".",10-time.elapsed), "\n")
          time.elapsed=time.elapsed+1
        }	
      } # end  for loop (i in 1:n.permutations)
      # GIORGIO UNCOMMENT
    } # end clust based perm and if paired == TRUE
    
    ##############################
    ## PREPARE RESULTS PERMUTATION
    ##############################
    # results are returned in this way:
    # - res.tmat = the observed tmat
    # - perm.or.filt.mat = a matrix with original dimentions with t-values that were significant, either for positive or for negative clusters.
    # - neg_p.values.mat = a matrix with original dimensions with permuted p values (negative)
    # - pos_p.values.mat = a matrix with original dimensions with permuted p values
    # - 
    
    ##
    # - first calculate the actual p value associated to each mass
    # - assign that p-values to all members of the cluster
    
    
    
    #### NEGATIVE RESULTS
    # create a mask of results to be applied to the matrix and the filt.mat
    # start from observed results
    if (!is.null(obs.negclust)){
      
      obs.neg.res.inclust=filt.mat
      # filter for cluster == 0 (i.e., exlude points not belonging to any cluster)
      # note the transpositioN! (obs.negclust is electrode*timepoints)
      obs.neg.res.inclust[t(obs.negclust)==0]=NA
      
      # transform to vector temporarily the results to use sapply
      obs.neg.res.inclust.vec=as.numeric(obs.neg.res.inclust)
      
      # calculate p-values as sum of times that the permuted values are smaller or equal than the observed value.
      # put in other terms, you calculate how unlikely is to observe your observed results.
      neg_p.values=sapply(obs.neg.res.inclust.vec, function(x){sum(clust.perm.max.res[2,]<=x)/length(clust.perm.max.res[2,])})
      
      # re-transform in matrix
      neg_p.values.mat=matrix(neg_p.values, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(or.res.t.mat)[2])
    } else {
      # this NA objects are created if there were NO significant values already in the observed data
      # matrix timepoinsts * electrode with NA
      neg_p.values.mat = matrix(NA, nrow=dim(filt.mat)[1], ncol=dim(filt.mat)[2])
      obs.neg.res.inclust.vec = rep(NA, times=length(unlist(res.t.mat)))
    }
    
    
    #### POSITIVE RESULTS
    # create a mask of results to be applied to the matrix and the filt.mat
    # start from observed results
    if (!is.null(obs.posclust)){
      
      obs.pos.res.inclust=as.matrix(filt.mat)
      # filter for cluster == 0 (i.e., exlude points not belonging to any cluster)
      # note the transpositioN! (obs.posclust is electrode*timepoints)
      obs.pos.res.inclust[t(obs.posclust)==0]=NA
      
      # transform to vector temporarily the results to use sapply
      obs.pos.res.inclust.vec=as.numeric(obs.pos.res.inclust)
      
      # calculate p-values as sum of values that are more positive or equal to the observed values in permutation
      # put in other terms, you calculate how unlikely is to observe your observed results.
      # IMPORTANT: I have to remove NA, otherwise if no cluster are found during permutation there is no mass of clusters
      # and so the value is NA.
      pos_p.values=sapply(obs.pos.res.inclust.vec, function(x){sum(clust.perm.max.res[1,]>=x, na.rm=TRUE)/length(clust.perm.max.res[1,])})
      
      # re-transform in matrix
      pos_p.values.mat=matrix(pos_p.values, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(or.res.t.mat)[2])
    } else {
      # see above, this is 
      pos_p.values.mat = matrix(NA, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(res.t.mat)[2])
      obs.pos.res.inclust.vec = rep(NA, times=length(unlist(res.t.mat)))
    }
    
    ##################################################
    # initialize objects for results
    perm.filt.mat = res.t.mat
    # Sub-STEP 1 -  # exclude values not belonging to any cluster
    # NOTE: this step should be meaningful only if you use the optional step 3) 
    # of cluster based permutation (removing points without neighbours)
    # otherwise every significnat t-point will always belong to a cluster 
    # (this follows the way clusters are formed)
    # first from original filter data( filtered only by the interval)
    # to do this I get the vectors
    # - first I create a dataset that puts (one beside the others) positive and negative results
    obs.pos.neg.res.inclust.dat=data.frame(obs.pos.res.inclust.vec, obs.neg.res.inclust.vec)
    # now I look for results that are NOT NA
    cluster.belong.log.vec=apply(obs.pos.neg.res.inclust.dat, 1, function(x){sum(x[!is.na(x)], na.rm=TRUE)!=0})
    # transform in matrix to be used as a mask to filter
    cluster.belong.log.mat=matrix(cluster.belong.log.vec, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(or.res.t.mat)[2])
    
    # mask my perm.data with only values that belong to clusters
    perm.filt.mat[!cluster.belong.log.mat]=NA
    
    # apply the new data to the NA matrix with original dimensions
    perm.or.filt.mat[startpoint:endpoint, electrodes]  = perm.filt.mat
    
    # Sub-STEP 2now I can filter the data for SIGNIFICANT clusters
    ## create a unique logical matrix to mask significant results (as in other functions)
    # NOTE that I call the object "temp.res" because it will have NA
    # and it is not how we want the final logical matrix (only TRUE/FALSE)
    
    temp.res.log.mat=neg_p.values.mat <p.crit  | pos_p.values.mat < p.crit
    
    ## with the following code# first I select all values who are TRUE in or.res.log.mat
    # and that are not NA. These are the significant results. Then I put !
    # in front of the selection to invert the selection (so I select all non-significant results)
    # and finaly I substitute NA in the matrix
    perm.filt.mat[!(temp.res.log.mat)&!is.na(temp.res.log.mat)]=NA
    
    #########################################################
    # PREPARE RESULTS WITH THE ORIGINAL SIZE
    #########################################################
    # in this section I start from the object already calculated and I return matrices with the same dimension
    # of the original supplied matrices. 
    # This is very useful for consistency and to keep the same dimensions across analysis.
    # in the results, the actual timepoints/points used are returned
    
    #######
    # initilize empty object for results. Note I start from or.res.t.mat, to be sure I have the right dimensions.
    generic.or.NA.mat = matrix(NA, nrow=dim(or.res.t.mat)[1], ncol=dim(or.res.t.mat)[2])
    generic.or.0.mat = matrix(0, nrow=dim(or.res.t.mat)[1], ncol=dim(or.res.t.mat)[2])
    
    colnames(generic.or.NA.mat)=colnames(or.res.t.mat)
    
    # t mat after permutation results
    perm.or.filt.mat=generic.or.NA.mat
    perm.or.filt.mat[startpoint:endpoint, electrodes]=perm.filt.mat
    perm.or.filt.mat=as.data.frame(perm.or.filt.mat)
    
    # positive cluster p-values
    or.pos.p.mat=generic.or.NA.mat
    or.pos.p.mat[startpoint:endpoint, electrodes]=pos_p.values.mat
    or.pos.p.mat = as.data.frame(or.pos.p.mat)
    
    # negative cluster p-values
    or.neg.p.mat=generic.or.NA.mat
    or.neg.p.mat[startpoint:endpoint, electrodes]=neg_p.values.mat
    or.neg.p.mat = as.data.frame(or.neg.p.mat)
    
    # fix exception for cluster
    # if no observed cluster is found the results is null (this is the output of find.clusters function if no cluster is found)
    # this would break the code below. So i create (in such cases) a dummy object with NA.
    
    if (is.null(obs.posclust)){
      obs.posclust=matrix(NA, nrow=dim(res.t.mat)[1], ncol=dim(res.t.mat)[2]) # 
    }
    
    if (is.null(obs.negclust)){
      obs.negclust=matrix(NA, nrow=dim(res.t.mat)[1], ncol=dim(res.t.mat)[2])  # 
    }
    
    
    
    
    
    # positive cluster labeling
    or.pos.clusters = generic.or.NA.mat
    or.pos.clusters[startpoint:endpoint, electrodes]=t(obs.posclust)
    or.pos.clusters = as.data.frame(or.pos.clusters)
    
    # negative cluster labeling
    or.neg.clusters = generic.or.NA.mat
    or.neg.clusters[startpoint:endpoint, electrodes]=t(obs.negclust)
    or.neg.clusters = as.data.frame(or.neg.clusters)
    
    
    
  } # end if method == cluster.based.permutations
  
  
  #######################################
  #### RETURN RESULTS
  #######################################
  
  # all cases excluded cluster based (which requires a different structure)
  if (p.adjust.method!="cluster.based.permutation"){
    
    param=data.frame( interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1], total.n.test=length(allres), p.correction=p.adjust.method)
    
    allresults=list(param=param, mass.t.results=or.filt.mat, sig=or.res.log.mat, t.mat=or.res.t.mat, p.mat=or.res.p.mat)
    
    if (stable.diff==TRUE){
      param=cbind(param, crit.msec=crit.msec, exact.crit.msec=exact.crit.msec)
      allresults=c(allresults, crit.list=list(stable.diff.res$crit.list))
    }
  }
  
  # case cluster based premutation
  if (p.adjust.method=="cluster.based.permutation"){
    param=data.frame( interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1], total.n.test=length(allres), p.correction=p.adjust.method)
    allresults=list(param=param, mass.t.results=perm.or.filt.mat, 
                    sig.pos= as.data.frame(or.pos.p.mat<p.crit), # calculate here on the fly
                    sig.neg = as.data.frame(or.neg.p.mat<p.crit), 
                    t.mat=or.res.t.mat, pos.p.mat = or.pos.p.mat, neg.p.mat=or.neg.p.mat, pos.clusters=or.pos.clusters, neg.clusters=or.neg.clusters)
    
  }
  
  invisible(allresults)
  
}



