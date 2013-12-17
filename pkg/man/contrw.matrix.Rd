\name{contrw.matrix}
\alias{contrw.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contrw.matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contrw.matrix(dep, subj, fac, dataset, corr = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dep}{
%%     ~~Describe \code{dep} here~~
}
  \item{subj}{
%%     ~~Describe \code{subj} here~~
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
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
function (dep, subj, fac, dataset, corr = F) 
{
    if (NA \%in\% dataset[, dep]) {
        NotAval = dataset[dataset[, dep] \%in\% NA, subj]
        stop(paste("le osservazioni con", subj, "=", NotAval, 
            "hanno dati mancanti!!!"), call. = FALSE)
    }
    dat = as.matrix(dataset[, fac])
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
    t.matrix = matrix(nrow = length(nuovo_fattore), ncol = length(nuovo_fattore), 
        dimnames = list(nuovo_fattore, nuovo_fattore))
    p.matrix = matrix(nrow = length(nuovo_fattore), ncol = length(nuovo_fattore), 
        dimnames = list(nuovo_fattore, nuovo_fattore))
    sig.matrix = matrix(data = "", nrow = length(nuovo_fattore), 
        ncol = length(nuovo_fattore), dimnames = list(nuovo_fattore, 
            nuovo_fattore))
    eff.matrix = matrix(nrow = length(nuovo_fattore), ncol = length(nuovo_fattore), 
        dimnames = list(nuovo_fattore, nuovo_fattore))
    critic = 0.05
    if (corr == TRUE) {
        critic = 0.05/(((length(nuovo_fattore) * length(nuovo_fattore)) - 
            length(nuovo_fattore))/2)
    }
    for (row in 1:length(nuovo_fattore)) {
        for (col in 1:length(nuovo_fattore)) {
            contr1 = as.character(nuovo_fattore[row])
            contr2 = as.character(nuovo_fattore[col])
            tabella = matrice[, c(contr1, contr2)]
            coefficients = c(rep(0, length(nuovo_fattore)))
            indici = c(1:length(nuovo_fattore))
            for (i in 1:length(contr1)) {
                coefficients[indici[nuovo_fattore == contr1[i]]] = 1/length(contr1)
            }
            for (i in 1:length(contr2)) {
                coefficients[indici[nuovo_fattore == contr2[i]]] = -1/length(contr2)
            }
            contr = coefficients
            y <- matrice \%*\% contr
            val.contr <- as.numeric(t.test(y)$estimate)
            tval <- as.numeric(t.test(y)$statistic)
            df.tval <- as.numeric(t.test(y)$parameter)
            p.tval <- as.numeric(t.test(y)$p.value)
            if (sum((coefficients)) != 0) {
                tval = NA
                p.tval = NA
            }
            t.matrix[row, col] = round(tval, 2)
            p.matrix[row, col] = round(p.tval, 3)
            if (p.tval < critic & !is.na(p.tval)) {
                sig.matrix[row, col] = "*"
            }
            if (sig.matrix[row, col] == "*") {
                eff.matrix[row, col] = round(val.contr, 3)
            }
        }
    }
    result = list(t.values = t.matrix, p.values = p.matrix, effect.matrix = eff.matrix, 
        sig = sig.matrix, df = df.tval, alpha = round(critic, 
            3))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
