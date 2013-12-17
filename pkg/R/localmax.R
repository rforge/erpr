#questa funzione, dato un vettore, trova il punto che può essere considerato come massimo locale. Rispetto alla definzione data da luck. Vale a dire un punto intorno massimo circondato da punti più bassi (n, nella funzione).
# di default n=5

localmax <- function(x, n=2){
	vet=x
	dat=data.frame(index=1:length(vet))
	# creo un data.frame con gli indici. Mi serve per utilizzare la funzione apply.
	
	test=function(index,vet,n) {
	indices=c((index-n):(index-1), (index+1):(index+n))
	indices=indices[indices>0&indices<(length(vet))]
	response=((!any(vet[indices]>vet[index]))&length(indices)==(n*2))
	return(response)
	}
	
	candidates.indices=apply(dat, 1, function(x){test(index=x, vet, n)})
	
	if(any(candidates.indices)){
	candidates=vet[candidates.indices]
	return(max(candidates)) } else {
		return(NA)
	}
	
	}
