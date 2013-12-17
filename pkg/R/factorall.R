factorall <-
function(x){
	for(i in 1:length(x)){
		if (is.factor(x[,i])){
			x[,i]=factor(x[,i])}
		}
		return(x)
	}
