
tpairs = function (dat, vars, contr, dep, wid, p.adjust.methods = "none", 
                   paired = "CHECK", ...) 
{
  dat$newfactor = apply(data.frame(dat[, vars]), 1, function(x) {
    paste(x, collapse = "_")
  })
  dat$newfactor = factor(dat$newfactor)
  dat = aggregate(dat[, dep], list(dat$newfactor, dat[, wid]), 
                  mean)
  names(dat) = c("newfactor", "wid", dep)
  contrast.names = NULL
  p.values = NULL
  t.values = NULL
  df = NULL
  mean.1 = NULL
  mean.2 = NULL
  pair=NULL
  if (contr[[1]][[1]] == "all") {
    combs = combn(levels(dat$newfactor), 2)
    contr = list(NULL)
    length(contr) = dim(combs)[2]
    for (k in 1:dim(combs)[2]) {
      contr[[k]] = combs[, k]
    }
  }
  for (i in 1:length(contr)) {
    
    # check within/between
    
    if (paired=="CHECK"){
      # check se within o between (if wid are the same)
      wid.diff=setdiff(dat[dat$newfactor %in% contr[[i]][[1]], "wid"],dat[dat$newfactor %in% contr[[i]][[2]], "wid"])
      wid.check=!length(wid.diff)>0
      
      if (wid.check==TRUE){
        res = t.test(dat[dat$newfactor %in% contr[[i]][[1]], 
                         dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                     paired = TRUE, ...)
        # aggiorno campo pair
        pair=c(pair, TRUE)
      }
      if (wid.check==FALSE){
        res = t.test(dat[dat$newfactor %in% contr[[i]][[1]], 
                         dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                     paired = FALSE, ...)
        pair=c(pair, FALSE)
      }
    }
    
    if (paired==TRUE|paired==FALSE){
      res = t.test(dat[dat$newfactor %in% contr[[i]][[1]], 
                       dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                   paired = paired, ...)
      pair=c(pair, paired)
    }
    contrast.names = c(contrast.names, paste(contr[[i]][[1]], 
                                             "vs", contr[[i]][[2]], sep = " "))
    p.values = c(p.values, res$p.value)
    t.values = c(t.values, res$statistic)
    df = c(df, res$parameter)
    mean.1 = c(mean.1, mean(dat[dat$newfactor %in% contr[[i]][[1]], 
                                dep]))
    mean.2 = c(mean.2, mean(dat[dat$newfactor %in% contr[[i]][[2]], 
                                dep]))
    
  }
  p.values.corr = signif(p.adjust(p.values, p.adjust.methods), 
                         2)
  
  # create splitted version of column names (a step back)
  # case 1 (more than one variable specified)
  contr = contrast.names
  
  if (length(vars)>1){
    contr_split = strsplit(as.character(contr), split= "_| ")
    contr_mat = matrix(unlist(contr_split), byrow=TRUE, nrow = length(contr))
    col_vs = length(vars)+1 # get the column with vs
    contr_vars = data.frame(contr_mat[, -col_vs]) # exclude it (and convert to df)
    old_vars_names = as.character ( t(sapply(vars, function(x){paste(x, 1:2, sep="_")})) )
    names(contr_vars) = old_vars_names
  }
  
  # case 2 (only one var specified)
  # it is necessary cause the data.frame contr_vars is built slightly differently.
  if (length(vars)==1){
    contr_split = strsplit(as.character(contr), split= " ")
    contr_mat = matrix(unlist(contr_split), byrow=TRUE, nrow = length(contr))
    col_vs = length(vars)+1 # get the column with vs
    contr_vars = data.frame(t(contr_mat[, -col_vs])) # exclude it (and convert to df)
    old_vars_names = as.character ( t(sapply(vars, function(x){paste(x, 1:2, sep="_")})) )
    names(contr_vars) = old_vars_names
  }
  
  
  results = data.frame(contr = contrast.names, p.value = p.values.corr, 
                       t.value = t.values, df = df, mean.1 = mean.1, mean.2, pair)
  
  # add initial columns
  results = cbind(results, contr_vars)
  
  attr(results, "p.corr") = p.adjust.methods
  cat("p values adjustment = ", p.adjust.methods, "\n")
  return(results)
}
