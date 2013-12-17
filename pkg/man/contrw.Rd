\name{contrw}
\alias{contrw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contrw
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
contrw(dep, subj, fac, contr1, contr2, dataset)
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
  \item{contr1}{
%%     ~~Describe \code{contr1} here~~
}
  \item{contr2}{
%%     ~~Describe \code{contr2} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
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
function (dep, subj, fac, contr1, contr2, dataset) 
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
    tabella = matrice[, colnames(matrice) \%in\% c(contr1, contr2)]
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
    cat("\n", "\n")
    cat("********************** Risultati Contrasti Within **************************", 
        "\n", "\n")
    cat("Contrasto inserito:    ", round(contr, 2), "\n\n")
    cat("Primo gruppo:   ", sort(paste(round(contr[contr > 0], 
        2), colnames(matrice)[contr > 0], sep = " "), decreasing = TRUE), 
        "\n")
    cat("Secondo gruppo:   ", sort(paste(round(contr[contr < 
        0], 2), colnames(matrice)[contr < 0], sep = " "), decreasing = TRUE), 
        "\n")
    cat("Media Primo gruppo:   ", mean(tabella[, contr1], na.rm = TRUE), 
        "\n")
    cat("Media Secondo gruppo:   ", mean(tabella[, contr2], na.rm = TRUE), 
        "\n\n")
    cat("Media del contrasto =", val.contr, "\n\n")
    cat("t =", tval, "     df = ", df.tval, "     p =", signif(p.tval, 
        2), "(due code)", "\n", "\n")
    cat("***********************************************************", 
        "\n", "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
