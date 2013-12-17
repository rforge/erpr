\name{contrb.matrix}
\alias{contrb.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contrb.matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contrb.matrix(dep, bet, dati, corr = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dep}{
%%     ~~Describe \code{dep} here~~
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
function (dep, bet, dati, corr = F) 
{
    dataset = dati
    dat = as.matrix(dataset[, bet])
    if (dim(dat)[2] > 1) {
        fattori = dat[, 1]
        for (i in 2:dim(dat)[2]) {
            fattori = paste(fattori, dat[, i], sep = "_")
        }
    }
    if (dim(dat)[2] == 1) {
        fattori = dat
    }
    dataset$nuovo_fattore = factor(fattori)
    livelli_nuovo_fattore = levels(factor(fattori))
    t.matrix = matrix(nrow = length(livelli_nuovo_fattore), ncol = length(livelli_nuovo_fattore), 
        dimnames = list(livelli_nuovo_fattore, livelli_nuovo_fattore))
    p.matrix = matrix(nrow = length(livelli_nuovo_fattore), ncol = length(livelli_nuovo_fattore), 
        dimnames = list(livelli_nuovo_fattore, livelli_nuovo_fattore))
    sig.matrix = matrix(data = "", nrow = length(livelli_nuovo_fattore), 
        ncol = length(livelli_nuovo_fattore), dimnames = list(livelli_nuovo_fattore, 
            livelli_nuovo_fattore))
    eff.matrix = matrix(nrow = length(livelli_nuovo_fattore), 
        ncol = length(livelli_nuovo_fattore), dimnames = list(livelli_nuovo_fattore, 
            livelli_nuovo_fattore))
    critic = 0.05
    if (corr == TRUE) {
        critic = 0.05/(((length(livelli_nuovo_fattore) * length(livelli_nuovo_fattore)) - 
            length(livelli_nuovo_fattore))/2)
    }
    for (row in 1:length(livelli_nuovo_fattore)) {
        for (cols in 1:length(livelli_nuovo_fattore)) {
            contr1 = as.character(livelli_nuovo_fattore[row])
            contr2 = as.character(livelli_nuovo_fattore[cols])
            livelli_nuovo_fattore = levels(as.factor(fattori))
            tabella = tapply(dataset[, dep], dataset[, "nuovo_fattore"], 
                mean)
            coefficients = c(rep(0, length(livelli_nuovo_fattore)))
            indici = c(1:length(livelli_nuovo_fattore))
            for (i in 1:length(contr1)) {
                coefficients[indici[livelli_nuovo_fattore == 
                  contr1]] = -1/length(contr1)
            }
            for (i in 1:length(contr1)) {
                coefficients[indici[livelli_nuovo_fattore == 
                  contr2]] = 1/length(contr2)
            }
            cb = coefficients
            fb <- dataset[, "nuovo_fattore"]
            dataset = dataset[order(dataset$nuovo_fattore), ]
            y <- dataset[, dep]
            if (!is.factor(fb)) 
                stop("Il primo vettore colonna della matrice dei dati non \303\213 un fattore!")
            vetcm <- rep((as.numeric(summary(fb))), (as.numeric(summary(fb))))
            vetcc <- rep(cb, (as.numeric(summary(fb))))
            num <- t(y) \%*\% (vetcc * vetcm^(-1))
            mod <- summary(aov(dataset[, dep] ~ nuovo_fattore, 
                dataset))
            varerr <- mod[[1]][2, 3]
            df.varerr <- mod[[1]][2, 1]
            correz <- ((as.numeric(summary(fb)))^(-1)) \%*\% ((cb)^2)
            den <- sqrt(varerr * correz)
            tval <- num/den
            pval <- (1 - pt(abs(tval), df.varerr)) * 2
            if (sum((coefficients)) != 0) {
                tval = NA
                pval = NA
            }
            t.matrix[row, cols] = round(tval, 2)
            p.matrix[row, cols] = round(pval, 3)
            if (pval < critic & !is.na(pval)) {
                sig.matrix[row, cols] = "*"
            }
        }
    }
    result = list(t.values = t.matrix, p.values = p.matrix, sig = sig.matrix, 
        df = df.varerr, alpha = round(critic, 3))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
