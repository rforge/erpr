char2fac <-
function(x){
	if (is.character(x)){
	x=as.factor(x)
	return(x)
	}
	if (is.data.frame(x)){
		for (i in 1:length(x)){
				if (is.character(x[,i])){
					x[,i]=as.factor(x[,i])
					}
		}
	}
return(x)
}
