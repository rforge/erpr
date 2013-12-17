contrb.matrix <-
function(dep,bet, dati, corr=F){
dataset=dati
dat=as.matrix(dataset[,bet])
	if (dim(dat)[2]>1) {
		fattori=dat[,1]
		for (i in 2:dim(dat)[2]){
			fattori=paste(fattori, dat[,i], sep="_")
			}
	}
	if (dim(dat)[2]==1) {
		fattori=dat
		}

dataset$nuovo_fattore=factor(fattori)
livelli_nuovo_fattore=levels(factor(fattori))

t.matrix=matrix(nrow=length(livelli_nuovo_fattore), ncol=length(livelli_nuovo_fattore), dimnames=list(livelli_nuovo_fattore, livelli_nuovo_fattore))

p.matrix=matrix(nrow=length(livelli_nuovo_fattore), ncol=length(livelli_nuovo_fattore), dimnames=list(livelli_nuovo_fattore, livelli_nuovo_fattore))

sig.matrix=matrix(data="",nrow=length(livelli_nuovo_fattore), ncol=length(livelli_nuovo_fattore), dimnames=list(livelli_nuovo_fattore, livelli_nuovo_fattore))

eff.matrix=matrix(nrow=length(livelli_nuovo_fattore), ncol=length(livelli_nuovo_fattore), dimnames=list(livelli_nuovo_fattore, livelli_nuovo_fattore))
# questa matrice mostra 1 se il primo membro del contrasto è maggiore del secondo. 2 in caso contrario. 

#####################
# SETTO ALPHA CRITICO
#####################
critic=0.05
if (corr==TRUE){
	critic=0.05/(((length(livelli_nuovo_fattore)*length(livelli_nuovo_fattore))-length(livelli_nuovo_fattore))/2) #correggo per il 	numero di contrasti (nota che non considero i contrasti che danno NA). Nota che faccio diviso 2 perché la matrice dei contrasti è simmetrica e altrimenti correggerei per il doppio dei contrasti effettuati. 
}	

for (row in 1:length(livelli_nuovo_fattore)){
	for (cols in 1:length(livelli_nuovo_fattore)){
		contr1=as.character(livelli_nuovo_fattore[row])
		contr2=as.character(livelli_nuovo_fattore[cols])
		livelli_nuovo_fattore=levels(as.factor(fattori))
		tabella=tapply(dataset[,dep], dataset[,"nuovo_fattore"], mean)
		coefficients=c(rep(0, length(livelli_nuovo_fattore)))
		indici=c(1:length(livelli_nuovo_fattore))
		for (i in 1:length(contr1)){
			coefficients[indici[livelli_nuovo_fattore==contr1]]=-1/length(contr1)
				}
		for (i in 1:length(contr1)){
			coefficients[indici[livelli_nuovo_fattore==contr2]]=1/length(contr2)
				}

cb=coefficients





### PARTE ADATTATA DA FUNZIONE GIANMARCO
## input
fb<-dataset[,"nuovo_fattore"]
dataset=dataset[order(dataset$nuovo_fattore),]
y<-dataset[,dep]
## controlli
if ( !is.factor(fb) )
stop("Il primo vettore colonna della matrice dei dati non Ë un fattore!")
## calcoli
vetcm<-rep((as.numeric(summary(fb))),(as.numeric(summary(fb))))
vetcc<-rep(cb,(as.numeric(summary(fb))))
num<-t(y)%*%(vetcc*vetcm^(-1))
mod<-summary(aov(dataset[,dep]~nuovo_fattore, dataset))
varerr<-mod[[1]][2,3]
df.varerr<-mod[[1]][2,1]
correz<- ((as.numeric(summary(fb)))^(-1)) %*% ((cb)^2)
den<-sqrt(varerr*correz)
tval<-num/den
pval<-(1-pt(abs(tval),df.varerr))*2
## ouput


	if (sum((coefficients))!=0){
			tval=NA
			pval=NA
		}
	t.matrix[row,cols]=round(tval,2)
	p.matrix[row,cols]=round(pval,3)
	
	if (pval<critic&!is.na(pval)){
		sig.matrix[row,cols]="*"
	}
	#if (sig.matrix[row,cols]=="*"){
	#	eff.matrix[row,cols]=round(val.contr,3)
	#	}	
		}#chiudo il ciclo cols
	}#chiudo il ciclo row
	result=list(t.values=t.matrix, p.values=p.matrix, sig=sig.matrix, df=df.varerr, alpha=round(critic,3))
	return(result)
}
