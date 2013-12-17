contrm.matrix <-
function(dep, subj , wit ,bet, dati, corr=FALSE) {
##questo primissimo ciclo permette di utilizzare dei dataset con selezioni (consente di evitare casini legati alla memoria di livelli di un fattore in cui alcuni livelli vengono esclusi)


dataset=dati
if(NA%in%dataset[,dep]) {
	NotAval=dataset[dataset[,dep]%in%NA,subj]
	stop(paste("le osservazioni con ", subj, "=",NotAval,"hanno dati mancanti!!!"), call.=FALSE)
	}
	
for (i in 1:dim(dataset)[2]) {
	if (is.factor(dataset[,i])) {
		dataset[,i]=as.factor(as.character(dataset[,i]))} #fattorizzo per rimuovere i livelli in più.
	}

if(length(bet>1)){
varsbet=as.matrix(dataset[,wit])
varsbet=apply(varsbet, 1, function(x){paste(x, collapse="_")})
}
if (length(bet)==1){
varsbet=dataset[,bet]	
}
if(length(wit>1)){
varswit=as.matrix(dataset[,wit])
varswit=apply(varswit, 1, function(x){paste(x, collapse="_")})
}
if (length(wit)==1){
varswit=dataset[,wit]	
}

varsmix=paste(varsbet, varswit, sep="-") #creo una variabiel che condensa wit e bet
fattoremix=levels(as.factor(varsmix)) 

t.matrix=matrix(nrow=length(fattoremix), ncol=length(fattoremix), dimnames=list(fattoremix, fattoremix))

p.matrix=matrix(nrow=length(fattoremix), ncol=length(fattoremix), dimnames=list(fattoremix, fattoremix))

sig.matrix=matrix(data="",nrow=length(fattoremix), ncol=length(fattoremix), dimnames=list(fattoremix, fattoremix))

# commento per ora questa parte. 
#eff.matrix=matrix(nrow=length(fattoremix), ncol=length(fattoremix), dimnames=list(fattoremix, fattoremix))

critic=0.05
if (corr==TRUE){
	critic=0.05/(((length(fattoremix)*length(fattoremix))-length(fattoremix))/2) #correggo per il 	numero di contrasti (nota che non considero i contrasti che danno NA). Nota che faccio diviso 2 perché la matrice dei contrasti è simmetrica e altrimenti correggerei per il doppio dei contrasti effettuati. 
}	

for (row in 1:length(fattoremix)){
	for (cols in 1:length(fattoremix)){
		
	bet1=strsplit(rownames(t.matrix)[row],"-")[[1]][1]
	bet2=strsplit(rownames(t.matrix)[cols],"-")[[1]][1]
	
	wit1=strsplit(rownames(t.matrix)[row],"-")[[1]][2]
	wit2=strsplit(rownames(t.matrix)[cols],"-")[[1]][2]
	
###in questa prima parte si ricodificano (le variabili) eventualemente collassando le diverse within
dat=as.matrix(dataset[,wit])
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
tabella=matrice[,colnames(matrice)%in%c(wit1,wit2)]
coefficients=c(rep(0, length(nuovo_fattore)))
indici=c(1:length(nuovo_fattore))
	
	for (i in 1:length(wit2)){
	coefficients[indici[nuovo_fattore==wit2[i]]]=-1/length(wit2)
	}
	for (i in 1:length(wit1)){
	coefficients[indici[nuovo_fattore==wit1[i]]]=1/length(wit1)
	}

contr=coefficients

###in questa prima parte si ricodificano (le variabili) eventualemente collassando le diverse between.
# Il problema è che non funziona se si selezionano tutti i livelli della variabile.

	dat2=as.matrix(dataset[,bet])
	if (dim(dat2)[2]>1) {
		fattori2=dat2[,1]
		for (i in 2:dim(dat2)[2]){
			fattori2=paste(fattori2, dat2[,i], sep="_")
			}
	}
	if (dim(dat2)[2]==1) {
		fattori2=dat2
		}
dataset$fattori2=as.factor(fattori2)
matrice2=unique(dataset[,c(paste(subj), "fattori2")])
matrice2=matrice2[order(matrice2[,1]),]
nuovo_fattore_bet_nome=paste(bet)
nuovo_fattore_bet=levels(dataset[,"fattori2"])
tabella2=matrice2[rownames(matrice2)%in%c(bet1,bet2),]
coefficients=c(rep(0, length(nuovo_fattore_bet)))
indici=c(1:length(nuovo_fattore_bet))
	for (i in 1:length(bet2)){
	coefficients[indici[nuovo_fattore_bet==bet2[i]]]=-1/length(bet2)
	}
	for (i in 1:length(bet1)){
	coefficients[indici[nuovo_fattore_bet==bet1[i]]]=1/length(bet1)
	}
contr_bet=coefficients







### questa seconda parte, presa dalla funzione di Gianmarco (e modificata nella parte dell'output), calcola i contrasti
# parte within
y<-matrice %*% contr
# parte between
y=cbind(y,matrice2)
y=y[order(y[,3]),]
y=y[,1]
vetcm<-rep((as.numeric(summary(matrice2$fattori2))),(as.numeric(summary(matrice2$fattori2))))
vetcc<-rep(contr_bet,(as.numeric(summary(matrice2$fattori2))))
num<-t(y)%*%(vetcc*vetcm^(-1))
mod<-summary(aov(y~matrice2$fattori2))
varerr<-mod[[1]][2,3]
df.varerr<-mod[[1]][2,1]
correz<- ((as.numeric(summary(matrice2$fattori2)))^(-1)) %*% ((contr_bet)^2)
den<-sqrt(varerr*correz)
tval<-num/den
pval<-(1-pt(abs(tval),df.varerr))*2
## output


	t.matrix[row,cols]=round(tval,2)
	p.matrix[row,cols]=round(pval,3)
	if (pval<critic&!is.na(pval)){
		sig.matrix[row,cols]="*"
	}

# commento la effect.matrix. Ce ne vorrebbero 2.una per i with, una per i bet
#	if (sig.matrix[row,cols]=="*"){
#		eff.matrix[row,cols]=0#round(val.contr,3)
#		}


		} #chiudo il ciclo row
		} #chiudo il ciclo col
diag(t.matrix)=NA
diag(p.matrix)=NA
diag(sig.matrix)=NA
result=list(t.values=t.matrix, p.values=p.matrix, sig=sig.matrix, df=df.varerr, alpha=round(critic,3))
	return(result)
	}
