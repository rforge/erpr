\name{contrm}
\alias{contrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contrm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contrm(dep, subj, wit, bet, wit1, wit2, bet1, bet2, dati, coeff.bet = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dep}{
%%     ~~Describe \code{dep} here~~
}
  \item{subj}{
%%     ~~Describe \code{subj} here~~
}
  \item{wit}{
%%     ~~Describe \code{wit} here~~
}
  \item{bet}{
%%     ~~Describe \code{bet} here~~
}
  \item{wit1}{
%%     ~~Describe \code{wit1} here~~
}
  \item{wit2}{
%%     ~~Describe \code{wit2} here~~
}
  \item{bet1}{
%%     ~~Describe \code{bet1} here~~
}
  \item{bet2}{
%%     ~~Describe \code{bet2} here~~
}
  \item{dati}{
%%     ~~Describe \code{dati} here~~
}
  \item{coeff.bet}{
%%     ~~Describe \code{coeff.bet} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dep, subj, wit, bet, wit1, wit2, bet1, bet2, dati, 
    coeff.bet = NULL) 
{
    dataset = dati
    if (NA \%in\% dataset[, dep]) {
        NotAval = dataset[dataset[, dep] \%in\% NA, subj]
        stop(paste("le osservazioni con ", subj, "=", NotAval, 
            "hanno dati mancanti!!!"), call. = FALSE)
    }
    for (i in 1:dim(dataset)[2]) {
        if (is.factor(dataset[, i])) {
            dataset[, i] = as.factor(as.character(dataset[, i]))
        }
    }
    dat = as.matrix(dataset[, wit])
    if (dim(dat)[2] > 1) {
        fattori = dat[, 1]
        for (i in 2:dim(dat)[2]) {
            fattori = paste(fattori, dat[, i], sep = "_")
        }
    }
    if (dim(dat)[2] == 1) {
        fattori = dat
    }
    matrice = as.matrix(tapply(dataset[, dep], list(dataset[, 
        subj], fattori), mean, na.rm = TRUE))
    nuovo_fattore = as.factor(colnames(matrice))
    tabella = matrice[, colnames(matrice) \%in\% c(wit1, wit2)]
    coefficients = c(rep(0, length(nuovo_fattore)))
    indici = c(1:length(nuovo_fattore))
    for (i in 1:length(wit2)) {
        coefficients[indici[nuovo_fattore == wit2[i]]] = -1/length(wit2)
    }
    for (i in 1:length(wit1)) {
        coefficients[indici[nuovo_fattore == wit1[i]]] = 1/length(wit1)
    }
    contr = coefficients
    dat2 = as.matrix(dataset[, bet])
    if (dim(dat2)[2] > 1) {
        fattori2 = dat2[, 1]
        for (i in 2:dim(dat2)[2]) {
            fattori2 = paste(fattori2, dat2[, i], sep = "_")
        }
    }
    if (dim(dat2)[2] == 1) {
        fattori2 = dat2
    }
    dataset$fattori2 = as.factor(fattori2)
    matrice2 = unique(dataset[, c(paste(subj), "fattori2")])
    matrice2 = matrice2[order(matrice2[, 1]), ]
    nuovo_fattore_bet_nome = paste(bet)
    nuovo_fattore = levels(dataset[, "fattori2"])
    tabella2 = matrice2[rownames(matrice2) \%in\% c(bet1, bet2), 
        ]
    coefficients = c(rep(0, length(nuovo_fattore)))
    indici = c(1:length(nuovo_fattore))
    for (i in 1:length(bet2)) {
        coefficients[indici[nuovo_fattore == bet2[i]]] = -1/length(bet2)
    }
    for (i in 1:length(bet1)) {
        coefficients[indici[nuovo_fattore == bet1[i]]] = 1/length(bet1)
    }
    contr_bet = coefficients
    if (!is.null(coeff.bet)) {
        contr_bet = coeff.bet
    }
    MediaGruppo1 = mean(dataset[fattori == wit1 & fattori2 == 
        bet1, dep], na.rm = TRUE)
    MediaGruppo2 = mean(dataset[fattori == wit2 & fattori2 == 
        bet2, dep], na.rm = TRUE)
    y <- matrice \%*\% contr
    y = cbind(y, matrice2)
    y = y[order(y[, 3]), ]
    y = y[, 1]
    vetcm <- rep((as.numeric(summary(matrice2$fattori2))), (as.numeric(summary(matrice2$fattori2))))
    vetcc <- rep(contr_bet, (as.numeric(summary(matrice2$fattori2))))
    num <- t(y) \%*\% (vetcc * vetcm^(-1))
    mod <- summary(aov(y ~ matrice2$fattori2))
    varerr <- mod[[1]][2, 3]
    df.varerr <- mod[[1]][2, 1]
    correz <- ((as.numeric(summary(matrice2$fattori2)))^(-1)) \%*\% 
        ((contr_bet)^2)
    den <- sqrt(varerr * correz)
    tval <- num/den
    pval <- (1 - pt(abs(tval), df.varerr)) * 2
    cat("\n", "\n")
    cat("********************** Riepilogo Contrasto Misto **************************", 
        "\n", "\n")
    cat("Primo gruppo within:   ", sort(paste(round(contr[contr > 
        0], 2), colnames(matrice)[contr > 0], sep = " "), decreasing = TRUE), 
        "\n")
    cat("Secondo gruppo within:   ", sort(paste(round(contr[contr < 
        0], 2), colnames(matrice)[contr < 0], sep = " "), decreasing = TRUE), 
        "\n", "\n")
    cat("Primo gruppo between:   ", sort(paste(round(contr_bet[contr_bet > 
        0], 2), paste(nuovo_fattore_bet_nome, levels(matrice2$fattori2)[contr_bet > 
        0], sep = " ")), decreasing = TRUE), "\n")
    if (sum(contr_bet) == 0) {
        cat("Secondo gruppo between:   ", sort(paste(round(contr_bet[contr_bet < 
            0], 2), paste(nuovo_fattore_bet_nome, levels(matrice2$fattori2)[contr_bet < 
            0], sep = " ")), decreasing = TRUE), "\n")
    }
    cat("\n")
    cat("Media Primo gruppo:   ", MediaGruppo1, "\n")
    cat("Media Secondo gruppo:   ", MediaGruppo2, "\n\n")
    cat("***********************************************************", 
        "\n")
    cat("********************** Risultati Contrasto Misto **************************", 
        "\n", "\n")
    cat("Contrasto within inserito:    ", contr, "\n")
    cat("Contrasto between inserito:    ", contr_bet, "\n")
    cat("Media del contrasto misto =", num, "\n")
    cat("t =", tval, "     df = ", df.varerr, "     p =", pval, 
        "(due code)", "\n", "\n")
    cat("***********************************************************", 
        "\n", "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
