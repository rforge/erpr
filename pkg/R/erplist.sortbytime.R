erplist.sortbytime = function(erplist=NULL, new.name=NULL){
  
  if (is.null(erplist)) {
    stop("an erplist object containing ERP data frames must be specified!", 
         call. = F)
  }
  
  if (is.null(erplist)) {
    stop("an erplist object containing ERP data frames must be specified!", 
         call. = F)
  }
  
  all_TrialTimes = NULL
  for (i in 1:length(erplist)){
    all_TrialTimes[i]=attr(erplist[[i]], "TrialTime")
  }
  if (any(is.null(all_TrialTimes))){
    stop(paste("The erplists:", which(is.null(all_TrialTimes)), "does not have the Trial time\n"), .call=F)
  }
  
  
  #checks 
  if(max(table(names(erplist))>1)){
    stop("The ERPlist supplied has duplicate names", call. = F)
    }
  
  # get all times
  TrialTimes_list = unlist( lapply(erplist, function(x){attr(x, "TrialTime")}) ) 
  
  # sort according to time
  TrialTimes_list_sorted = sort(TrialTimes_list)
  
  # sort erplist according to names of sorted TrialTime
  sorted_erplist = erplist[ names(TrialTimes_list_sorted) ]
  
  ################
  # change names
  ################
  if (is.null(new.name)) {
    # retrieve original base
    or.bases = gsub("_[0-9]+$", "", names(sorted_erplist))
    names(sorted_erplist)=paste(or.bases, 1:length(sorted_erplist), sep="_")
  } else {
    names(sorted_erplist)=paste(new.name, 1:length(sorted_erplist), sep="_")
  }
  
  
  
  
  
  return(sorted_erplist)
  
}