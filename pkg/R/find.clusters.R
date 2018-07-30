find.clusters<-function(tscores, thresh, chan_hood, thresh_sign, min_nchans){
  
  ### NOTE! the code of find cluster is below.
  # I embed before the code for follow_clust, an internal function
  
  follow_clust<-function(n_above,current_voxel_id,current_clust_num,above_thresh_ids,above_thresh_times,above_thresh_chans,chan_hood,n_chan){
    
    # NOTE: 25/07/2018 added n_chan, because returned errors
    # a = 5
    
    new_members=matrix(0, nrow=1, ncol=n_chan*3) 
    new_members_ct=0
    
    #
    #assign("clust_ids", clust_ids, envir=globalenv())
    
    for (b in current_clust_num : n_above){
      
      if (!clust_ids[above_thresh_ids[b]]){
        
        temp_dist=abs(above_thresh_times[b]-above_thresh_times[current_voxel_id]);
        
        if (above_thresh_chans[current_voxel_id]==above_thresh_chans[b]){
          #voxels are at same channel
          chan_dist=0;
          
        } else if (chan_hood[above_thresh_chans[current_voxel_id] , above_thresh_chans[b]]) {
          #channels are neighbors
          chan_dist=1;
        } else {
          #voxels aren't spatially compatible
          chan_dist=2
        }
        
        if ((temp_dist+chan_dist)<=1) {
          #if voxels are at same time point and neighboring channels OR
          #if voxels are at same channel and neighboring time points,
          #merge them into the same cluster
          
          clust_ids[above_thresh_ids[b]]<<-current_clust_num;
          #keep track of which other voxels are joined to this
          #cluster
          new_members_ct=new_members_ct+1;
          new_members[new_members_ct]=b;
        } # if ((temp_dist)
      }# if !clust
    } # for (b)
    
    return(new_members)
  } # end function follow_clust
  
  
  
  
  
  
  # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/find_clusters.m
  
  
  #tscores=t(res$t.mat)
  
  #thresh=5
  
  #thresh_sign=-1 # metto un numero positivo
  
  
  # !!! Data ara arranged nchan*n_tpt
  
  
  n_chan=dim(tscores)[1] # get number of channels
  n_tpt=dim(tscores)[2] # get number of timepoints
  
  # !! I create a clust_ids variable in the Global environment (to be fixed)
  
  clust_ids<<-matrix(0, nrow=n_chan, ncol=n_tpt)
  
  time_matrix=t( replicate(n_chan, 1:n_tpt) ) # matrix 1:nchan * timepoints  (chans in riga)
  chan_matrix= replicate(n_tpt, 1:n_chan)  # crate matrix nchan * 1:timepoints (timepoints in colonna)
  
  if (thresh_sign > 0){
    
    #looking for positive clusters
    above_thresh_ids=which(tscores>=thresh);
    
  } else {
    
    #looking for negative clusters
    above_thresh_ids=which(tscores<=thresh);
  }
  
  above_thresh_times=time_matrix[above_thresh_ids];
  above_thresh_chans=chan_matrix[above_thresh_ids];
  
  # Clear chan & time matrix to save memory (just in case)
  rm(chan_matrix, time_matrix)
  
  n_above=length(above_thresh_ids);
  
  n_clust=0
  
  # perform the search only if n_above is > 0
  # (only if there are actually values above the thresholds)
  
  if (n_above > 0) {
    
    for (a in 1:n_above){
      
      voxel_id=above_thresh_ids[a];
      
      # if there is an NA, it is considered as an already checked voxel
      # GIORGIO: not sure this is correct or there is a bug in the code
      
      if (is.na(clust_ids[voxel_id])){
        
        clust_ids[voxel_id]=1
        
      }
      
      ## end part added to avoid crashes
      
      if(!clust_ids[voxel_id]){
        # this "if" goes if the "voxel" isn't in a cluster yet
        n_clust=n_clust+1;
        
        clust_ids[voxel_id]<<-n_clust;
        
        #go through all the remaining voxels and find all the above
        #threshold voxels this voxel is neighbors with and give them this
        #cluster # G: initially the not_checked voxels are all those above the thresh
        voxels_not_checked = rep(1, length(above_thresh_ids));
        
        check_me = rep(0, length(above_thresh_ids));
        
        check_me[a] = 1;
      } # close if !clust_ids
      
      while (sum(check_me>0)) {
        
        # find the indices of the non-zero elements and return only the first
        first = which(check_me>0)[1]
        new = follow_clust(n_above,first,n_clust,above_thresh_ids,above_thresh_times,above_thresh_chans,chan_hood,n_chan)
        check_me[new]=1
        voxels_not_checked[first]=0 # G: set to 0 the first voxel, now it is checked.
        check_me = check_me&voxels_not_checked # G: the voxel to be checked are those "not checked" & with "check me".
        
      } # close while
      
    } # close for( a in 1:n_above)
    
    # NOTE: this works also in the case no cluster above threshold are found
    clust_membership=clust_ids;
    
    #clear global clust_ids
    # TO BE DONE
    
    # exclude clusters without a minimun number of neighbours
    if(!is.null(min_nchans)){
      # get cluster values
      clust_values=unique(as.numeric(clust_membership))
      # exclude 0 (i.e., no cluster)
      clust_values=clust_values[clust_values!=0]
      
      # loop and if therea not a least two channels (rows) with 
      # at least a significant result, exclude that cluster
      
      for (iClust in 1:length(clust_values)){
        # initialize
        curr_clust=clust_membership
        curr_clust[curr_clust!=clust_values[iClust]]=NA
        curr_clust=curr_clust!=0 # transform in logical matrix (so 1 means there is a cluster)
        curr_check=colSums(curr_clust, na.rm=TRUE)
        
        # if there are not at least min_nchans sign sensors or if for some reasons all check are NA
        if (all(curr_check<min_nchans)|all(is.na(curr_check))){
          # set 0 to this cluster
          clust_membership[clust_membership==clust_values[iClust]]=0
        }
      }
      # transform to NULL if all clusters are excluded
      if(all(as.numeric(clust_membership)==0)){
        clust_membership=NULL
      }
      
    }
    
    
    # NOTE GIORGIO: now only one out of two results is returned and NOT n_clust
    return(clust_membership)
    
  } # end if n_above > 0

} # end function find_cluster