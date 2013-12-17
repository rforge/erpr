contrw.matrix <-
function(dep, subj , fac, dataset, corr=F){
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
#la parte che segue attualemnte è fatta tramite un ciclo. Sicuramente può essere migliorata tramite operazioni con matrici. Ma ho fretta....(16/09/2010).

t.matrix=matrix(nrow=length(nuovo_fattore), ncol=length(nuovo_fattore), dimnames=list(nuovo_fattore, nuovo_fattore))

p.matrix=matrix(nrow=length(nuovo_fattore), ncol=length(nuovo_fattore), dimnames=list(nuovo_fattore, nuovo_fattore))

sig.matrix=matrix(data="",nrow=length(nuovo_fattore), ncol=length(nuovo_fattore), dimnames=list(nuovo_fattore, nuovo_fattore))

eff.matrix=matrix(nrow=length(nuovo_fattore), ncol=length(nuovo_fattore), dimnames=list(nuovo_fattore, nuovo_fattore))
# questa matrice mostra 1 se il primo membro del contrasto è maggiore del secondo. 2 in caso contrario. 

#####################
# SETTO ALPHA CRITICO
#####################
critic=0.05
if (corr==TRUE){
	critic=0.05/(((length(nuovo_fattore)*length(nuovo_fattore))-length(nuovo_fattore))/2) #correggo per il 	numero di contrasti (nota che non considero i contrasti che danno NA). Nota che faccio diviso 2 perché la matrice dei contrasti è simmetrica e altrimenti correggerei per il doppio dei contrasti effettuati. 
}	

for (row in 1:length(nuovo_fattore)){
	for (col in 1:length(nuovo_fattore)){
		contr1=as.character(nuovo_fattore[row])
		contr2=as.character(nuovo_fattore[col])
		tabella=matrice[,c(contr1,contr2)]
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



		if (sum((coefficients))!=0){
			tval=NA
			p.tval=NA
		}
	t.matrix[row,col]=round(tval,2)
	p.matrix[row,col]=round(p.tval,3)
	if (p.tval<critic&!is.na(p.tval)){
		sig.matrix[row,col]="*"
	}
	if (sig.matrix[row,col]=="*"){
		eff.matrix[row,col]=round(val.contr,3)
		}	
	}#chiudo il ciclo col
	}#chiudo il ciclo row
	result=list(t.values=t.matrix, p.values=p.matrix,effect.matrix=eff.matrix, sig=sig.matrix, df=df.tval, alpha=round(critic,3))
	return(result)
}
