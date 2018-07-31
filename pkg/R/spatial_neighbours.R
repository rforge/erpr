spatial_neighbours=function(chanlocs, max_dist, head_radius=NULL){
  
  # chanlocs=topoplot(return.coord=T)
  # max_dist= 3 # 
  
  # this function is an R translation of spatial_neighbours
  # by David Groppe in MASS univariate toolbox.
  # all credits to him, all blames to me.
  
  # get numnber of electrode (number fo rows of chanloc data.frame)
  # e.g. from topoplot.
  
  n_chan=dim(chanlocs)[1];
  
  if (is.null(head_radius)){a=
    cat("Estimating the radius of the head by assuming that the center of the head is [0,0,0] in Cartesian coordinates.\n");
  
  dst_from_origin=rep(0,n_chan);
  
  for (a in 1:n_chan){
    dst_from_origin[a]=sqrt(sum(c(chanlocs$x[a], chanlocs$y[a],chanlocs$z[a])^2));
  }
  
  mn=min(dst_from_origin);
  mx=max(dst_from_origin);
  md=median(dst_from_origin);
  cat("Min/Max electrode distance from origin (in chanlocs units):",mn,"-", mx, "\n");
  uni=unique(dst_from_origin);
  if (((mx-mn)/md)>0.001) {
    cat("WARNING: It appears that all electrodes are NOT the same distance from the origin!!!\n");
    cat("Your electrodes Cartesian coordinates either are not spherical or are not centered on [0 0 0].\n");
    cat("This function will still work properly but its estimate of the radius of the head of will not be correct.\n");
  }
  
  head_radius=median(dst_from_origin);
  cat("Radius of head (in chanlocs units) is estimated to be ",head_radius, "\n");
  
  } else {
    cat("Using provided head radius of", head_radius, "(in chanlocs units)\n");
  }
  
  
  circumference=56; #very rough estimate based on the average circumference of 10 Kutaslab participants
  max_dist_cm=max_dist*circumference/(2*pi*head_radius); #Radius=Circumference/(2*pi)
  
  cat("max_dist value of", max_dist, "corresponds to an approximate distance of", max_dist_cm);
  cat(" assuming a 56 cm great circle circumference head and that your electrode coordinates are based on an idealized\n");
  cat("  spherical head with radius of", head_radius, "\n");
  
  chan_hood=matrix(0, nrow=n_chan,ncol=n_chan);
  n_neighbors=rep(0,n_chan);
  chan_dist=rep(1,n_chan*(n_chan-1)/2);
  ct=0;
  for (mychan in 1:n_chan){
    coordA=c(chanlocs$x[mychan], chanlocs$y[mychan], chanlocs$z[mychan]);
    for (d in mychan:n_chan){
      coordB=c(chanlocs$x[d], chanlocs$y[d], chanlocs$z[d]);
      dstnce=sqrt(sum((coordA-coordB)^2));
      if (dstnce<=max_dist) {
        chan_hood[mychan,d]=1;
        chan_hood[d,mychan]=1;
      } # close if dstnce
      if (mychan!=d){ 
        #don"t count channels with themselves
        ct=ct+1;
        chan_dist[ct]=dstnce;
      } # close if (mychan!=d)
    } # close loop for (d ...
    n_neighbors[mychan]=sum(chan_hood[mychan, ])-1;
  } # close loop for(mychan...
  
  cat("Min/Max distances between all pairs of channels (in chanlocs units): ",
      min(chan_dist)," ", max(chan_dist), "\n");
  cat("Median (semi-IQR) distance between all pairs of channels (in chanlocs units):", 
      median(chan_dist), "-", IQR(chan_dist)/2, "\n");
  cat("Mean (SD) # of neighbors per channel: ",  mean(n_neighbors), " ", sd(n_neighbors), "\n");
  cat("Median (semi-IQR) # of neighbors per channel: ", median(n_neighbors), " ", IQR(n_neighbors)/2, "\n");
  cat("Min/max # of neighbors per channel: ", min(n_neighbors), "-", max(n_neighbors), "\n");
  
  return(chan_hood)
  
  
}