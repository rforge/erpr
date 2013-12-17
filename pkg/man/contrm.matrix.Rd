\name{contrm.matrix}
\alias{contrm.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contrm.matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contrm.matrix(dep, subj, wit, bet, dati, corr = FALSE)
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
  \item{dati}{
%%     ~~Describe \code{dati} here~~
}
  \item{corr}{
%%     ~~Describe \code{corr} here~~
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
function (dep, subj, wit, bet, dati, corr = FALSE) 
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
    if (length(bet > 1)) {
        varsbet = as.matrix(dataset[, wit])
        varsbet = apply(varsbet, 1, function(x) {
            paste(x, collapse = "_")
        })
    }
    if (length(bet) == 1) {
        varsbet = dataset[, bet]
    }
    if (length(wit > 1)) {
        varswit = as.matrix(dataset[, wit])
        varswit = apply(varswit, 1, function(x) {
            paste(x, collapse = "_")
        })
    }
    if (length(wit) == 1) {
        varswit = dataset[, wit]
    }
    varsmix = paste(varsbet, varswit, sep = "-")
    fattoremix = levels(as.factor(varsmix))
    t.matrix = matrix(nrow = length(fattoremix), ncol = length(fattoremix), 
        dimnames = list(fattoremix, fattoremix))
    p.matrix = matrix(nrow = length(fattoremix), ncol = length(fattoremix), 
        dimnames = list(fattoremix, fattoremix))
    sig.matrix = matrix(data = "", nrow = length(fattoremix), 
        ncol = length(fattoremix), dimnames = list(fattoremix, 
            fattoremix))
    critic = 0.05
    if (corr == TRUE) {
        critic = 0.05/(((length(fattoremix) * length(fattoremix)) - 
            length(fattoremix))/2)
    }
    for (row in 1:length(fattoremix)) {
        for (cols in 1:length(fattoremix)) {
            bet1 = strsplit(rownames(t.matrix)[row], "-")[[1]][1]
            bet2 = strsplit(rownames(t.matrix)[cols], "-")[[1]][1]
            wit1 = strsplit(rownames(t.matrix)[row], "-")[[1]][2]
            wit2 = strsplit(rownames(t.matrix)[cols], "-")[[1]][2]
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
            tabella = matrice[, colnames(matrice) \%in\% c(wit1, 
                wit2)]
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
            nuovo_fattore_bet = levels(dataset[, "fattori2"])
            tabella2 = matrice2[rownames(matrice2) \%in\% c(bet1, 
                bet2), ]
            coefficients = c(rep(0, length(nuovo_fattore_bet)))
            indici = c(1:length(nuovo_fattore_bet))
            for (i in 1:length(bet2)) {
                coefficients[indici[nuovo_fattore_bet == bet2[i]]] = -1/length(bet2)
            }
            for (i in 1:length(bet1)) {
                coefficients[indici[nuovo_fattore_bet == bet1[i]]] = 1/length(bet1)
            }
            contr_bet = coefficients
            y <- matrice \%*\% contr
            y = cbind(y, matrice2)
            y = y[order(y[, 3]), ]
            y = y[, 1]
            vetcm <- rep((as.numeric(summary(matrice2$fattori2))), 
                (as.numeric(summary(matrice2$fattori2))))
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
            t.matrix[row, cols] = round(tval, 2)
            p.matrix[row, cols] = round(pval, 3)
            if (pval < critic & !is.na(pval)) {
                sig.matrix[row, cols] = "*"
            }
        }
    }
    diag(t.matrix) = NA
    diag(p.matrix) = NA
    diag(sig.matrix) = NA
    result = list(t.values = t.matrix, p.values = p.matrix, sig = sig.matrix, 
        df = df.varerr, alpha = round(critic, 3))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
