\name{ezANOVA.nout}
\alias{ezANOVA.nout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ezANOVA.nout
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ezANOVA.nout(data, out, n, p.crit = 0.05, plot = FALSE, plot.par = NULL, cex.x, plot.vars = "all", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{out}{
%%     ~~Describe \code{out} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{p.crit}{
%%     ~~Describe \code{p.crit} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{plot.par}{
%%     ~~Describe \code{plot.par} here~~
}
  \item{cex.x}{
%%     ~~Describe \code{cex.x} here~~
}
  \item{plot.vars}{
%%     ~~Describe \code{plot.vars} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (data, out, n, p.crit = 0.05, plot = FALSE, plot.par = NULL, 
    cex.x, plot.vars = "all", ...) 
{
    out = factor(data[, paste(out)])
    out.s = combn(levels(out), length(levels(out)) - n)
    cat("  ", dim(out.s)[2], "possibile combinations will be evaluated\n")
    counter = 0
    for (i in 1:dim(out.s)[2]) {
        dat = data[out \%in\% out.s[, i], ]
        dat = factorall(dat)
        res = ezANOVA(data = dat, ...)
        res.sig = ezresults(res)
        if (any(res.sig$p < p.crit)) {
            counter = counter + 1
            if (counter == 1) {
                res.list = list(list(out = levels(out)[!levels(out) \%in\% 
                  out.s[, i]], res.sig))
            }
            if (counter > 1) {
                res.list[[counter]] = list(levels(out)[!levels(out) \%in\% 
                  out.s[, i]], res.sig)
            }
        }
    }
    alleffects = res$ANOVA$Effect
    attr(res.list, "effects") = alleffects
    attr(res.list, "out") = out
    if (plot == TRUE) {
        if (plot.vars != "all") {
            alleffects = plot.vars
        }
        if (!is.null(plot.par)) {
            par(mfrow = c(plot.par))
        }
        else {
            par(mfrow = c(1, length(alleffects)))
        }
        for (k in 1:length(alleffects)) {
            eff = lapply(res.list, function(x) {
                grep(paste("^", alleffects[k], "$", sep = ""), 
                  x[[2]]$Effect)
            })
            list.sig = res.list[!is.na(eff > 1)]
            list.sig.out = lapply(list.sig, function(x) {
                x[[1]]
            })
            sig.out = unlist(list.sig.out)
            table.sig.out = table(sig.out)
            add.names = levels(out)[!levels(out) \%in\% names(table.sig.out)]
            add.matrix = matrix(0, ncol = length(add.names), 
                nrow = 1)
            if (dim(add.matrix)[2] > 0) {
                final.matrix = cbind(add.matrix, t(matrix(table.sig.out)))
                colnames(final.matrix) = c(add.names, names(table.sig.out))
            }
            if (dim(add.matrix)[2] == 0) {
                final.matrix = t(matrix(table.sig.out))
                colnames(final.matrix) = names(table.sig.out)
            }
            barplot(final.matrix, main = alleffects[k], col = "gray", 
                cex.names = cex.x)
        }
    }
    return(res.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
