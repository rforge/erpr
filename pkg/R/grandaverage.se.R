 grandaverage.se = function (base, numbers, electrodes = "all", erplist = NULL, startmsec=NULL, endmsec=NULL, 
                  NA.sub = TRUE, type = "se") 
{
  if (is.null(erplist)) {
    stop("an erplist object containing ERP data frames must be specified!", 
         call. = F)
  }
  object.names = c(paste(base, numbers, sep = ""))
  if (any(!object.names %in% names(erplist))) {
    missing.objects = object.names[!object.names %in% names(erplist)]
    missing.object.collist = paste(missing.objects, "\n", 
                                   sep = "")
    stop("The following objects are not contained in the erplist specified:\n", 
         missing.object.collist, call. = F)
  }
  
  ### get startmsec from the first object
  erpdf = erplist[[1]]
  
  if(!is.null(attr(erpdf, "startmsec")) & !is.null(attr(erpdf, "endmsec"))){
    startmsec=attr(erpdf, "startmsec")
    endmsec=attr(erpdf, "endmsec")
  }
  
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  
  comment_text = paste("Subjects averaged: ", paste(base, numbers[1], 
                                                    sep = ""))
  average.temp = erplist[[paste(base, numbers[1], sep = "")]]
  if (electrodes[1] == "all") {
    electrodes = names(average.temp)
  }
  average.temp = average.temp[, electrodes]
  noNA.num = apply(average.temp, 2, function(x) {
    as.numeric(!all(is.na(x)))
  })
  if (NA.sub == TRUE) {
    average.temp[is.na(average.temp)] = 0
  }
  for (i in 2:length(numbers)) {
    average.temp.new = erplist[[paste(base, numbers[i], sep = "")]][, 
                                                                    electrodes]
    if (NA.sub == TRUE) {
      average.temp.new[is.na(average.temp.new)] = 0
    }
    average.temp = average.temp + average.temp.new
    comment_text = paste(comment_text, paste(base, numbers[i], 
                                             sep = ""), "\n")
  
    # count (and store), if there are NA
      noNA.num.new = apply(average.temp.new, 2, function(x) {
      as.numeric(!all(is.na(x)))
    })
      
    noNA.num = rbind(noNA.num, noNA.num.new)
  }
  
  electrodes.n = colSums(noNA.num) # electrodes.n is the sum of subjects with no NA (for each electrode)
  
  # calculate average
  average = average.temp/rep(electrodes.n, each = nrow(average.temp))

  
  # calculate sd
  average.temp2 = (erplist[[paste(base, numbers[1], sep = "")]] - average)^2
  
  for (i in 2:length(numbers)) {
    average.temp2.new = (erplist[[paste(base, numbers[i], sep = "")]][,  electrodes] - average)^2
    
    if (NA.sub == TRUE) {
      average.temp2.new[is.na(average.temp2.new)] = 0
    }
    
    average.temp2 = average.temp2 + average.temp2.new
    
  }
  # NOTE: I don't re-calculate electrodes.n, cause I already calculated it for average (and it's the same)
  
  se = sqrt( average.temp2/ ( rep(electrodes.n, each = nrow(average.temp)) -1) )/ sqrt( rep(electrodes.n, each = nrow(average.temp)))
  
  se = as.data.frame(se)
  
  comment(se) = comment_text
  
  # add startmsec and enmdsec
  attr(se, "startmsec")=startmsec
  attr(se, "endmsec")=endmsec
  
  
  if (sum(electrodes.n - (length(numbers))) != 0) {
    warning("The average included some NA values.", call. = FALSE)
  }
  return(se)
}