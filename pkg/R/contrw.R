contrw <-
function(dep, subj , fac , contr1,contr2, dataset){
###in questa prima parte si ricodificano (le variabili) eventualemente collassando le diverse within
	if(NA%in%dataset[,dep]) {
	NotAval=dataset[dataset[,dep]%in%NA,subj]
	stop(paste("le osservazioni con", subj, "=",NotAval,"hanno dati mancanti!!!"), call.=FALSE)
	}
	dat=as.matrix(dataset[,fac])
	if (dim(dat)[2]>1) {
		fattori=dat[,1]
		for (i in 2:dim(dat)[2]){
			fattori=paste(fattori, dat[,i], sep="_")
			}
	}
	if (dim(dat)[2]==1) {
		fattori=dat
		}
matrice=as.matrix(tapply(dataset[, dep], list(dataset[, subj], fattori), mean, na.rm=TRUE))
nuovo_fattore=as.factor(colnames(matrice))
tabella=matrice[,colnames(matrice)%in%c(contr1,contr2)]
coefficients=c(rep(0, length(nuovo_fattore)))
indici=c(1:length(nuovo_fattore))
	for (i in 1:length(contr1)){
	coefficients[indici[nuovo_fattore==contr1[i]]]=1/length(contr1)
	}
	for (i in 1:length(contr2)){
	coefficients[indici[nuovo_fattore==contr2[i]]]=-1/length(contr2)
	}
contr=coefficients
### questa seconda parte, presa dalla funzione di Gianmarco (e modificata nella parte dell'output), calcola i contrasti
	y<-matrice %*% contr
	val.contr<-as.numeric(t.test(y)$estimate)
	tval<-as.numeric(t.test(y)$statistic)
	df.tval<-as.numeric(t.test(y)$parameter)
	p.tval<-as.numeric(t.test(y)$p.value)
	##output
	cat("\n","\n")
	cat("********************** Risultati Contrasti Within **************************","\n","\n")
	cat("Contrasto inserito:    ",round(contr,2),"\n\n")
	cat("Primo gruppo:   ",sort(paste(round(contr[contr>0],2),colnames(matrice)[contr>0], sep=" "), decreasing=TRUE),"\n")
	cat("Secondo gruppo:   ",sort(paste(round(contr[contr<0],2),colnames(matrice)[contr<0], sep=" "), decreasing=TRUE),"\n")
	cat("Media Primo gruppo:   ",mean(tabella[,contr1], na.rm=TRUE),"\n")
	cat("Media Secondo gruppo:   ",mean(tabella[,contr2], na.rm=TRUE),"\n\n")
	cat("Media del contrasto =",val.contr,"\n\n")
	cat("t =",tval,"     df = ",df.tval,"     p =",signif(p.tval,2),"(due code)","\n","\n")
	cat("***********************************************************","\n","\n")
}
