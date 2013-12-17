\name{plot.nout}
\alias{plot.nout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.nout
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.nout(effects = "all", res.list, p.crit = 0.05, plot.par = NULL, cex.x = 1, plot.vars = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{effects}{
%%     ~~Describe \code{effects} here~~
}
  \item{res.list}{
%%     ~~Describe \code{res.list} here~~
}
  \item{p.crit}{
%%     ~~Describe \code{p.crit} here~~
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
function (effects = "all", res.list, p.crit = 0.05, plot.par = NULL, 
    cex.x = 1, plot.vars = "all") 
{
    out = attr(res.list, "out")
    if (length(effects) == 1 & effects[1] == "all") {
        alleffects = attr(res.list, "effects")
    }
    else {
        alleffects = effects
    }
    par(mfrow = c(1, length(alleffects)))
    if (!is.null(plot.par)) {
        par(mfrow = c(plot.par))
    }
    if (plot.vars != "all") {
        alleffects = plot.vars
    }
    for (k in 1:length(alleffects)) {
        eff = lapply(res.list, function(x) {
            grep(paste("^", alleffects[k], "$", sep = ""), x[[2]]$Effect)
        })
        list.sig = res.list[!is.na(eff > 1)]
        list.sig.out = lapply(list.sig, function(x) {
            x[[1]]
        })
        sig.out = unlist(list.sig.out)
        table.sig.out = table(sig.out)
        add.names = levels(out)[!levels(out) \%in\% names(table.sig.out)]
        add.matrix = matrix(0, ncol = length(add.names), nrow = 1)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
