\name{t.test.pairs}
\alias{t.test.pairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
t.test.pairs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
t.test.pairs(dat, vars, contr, dep, wid, p.adjust.methods = "none", paired = F, type = "t.test", vars.bet = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{vars}{
%%     ~~Describe \code{vars} here~~
}
  \item{contr}{
%%     ~~Describe \code{contr} here~~
}
  \item{dep}{
%%     ~~Describe \code{dep} here~~
}
  \item{wid}{
%%     ~~Describe \code{wid} here~~
}
  \item{p.adjust.methods}{
%%     ~~Describe \code{p.adjust.methods} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{vars.bet}{
%%     ~~Describe \code{vars.bet} here~~
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
function (dat, vars, contr, dep, wid, p.adjust.methods = "none", 
    paired = F, type = "t.test", vars.bet = NULL, ...) 
{
    dat$newfactor = apply(data.frame(dat[, vars]), 1, function(x) {
        paste(x, collapse = "_")
    })
    dat$newfactor = factor(dat$newfactor)
    dat = aggregate(dat[, dep], list(dat$newfactor, dat[, wid]), 
        mean)
    names(dat) = c("newfactor", "wid", dep)
    contrast.names = NULL
    p.values = NULL
    t.values = NULL
    df = NULL
    mean.1 = NULL
    mean.2 = NULL
    if (contr[[1]][[1]] == "all") {
        combs = combn(levels(dat$newfactor), 2)
        contr = list(NULL)
        length(contr) = dim(combs)[2]
        for (k in 1:dim(combs)[2]) {
            contr[[k]] = combs[, k]
        }
    }
    for (i in 1:length(contr)) {
        res = t.test(dat[dat$newfactor \%in\% contr[[i]][[1]], 
            dep], dat[dat$newfactor \%in\% contr[[i]][[2]], dep], 
            paired = paired, ...)
        contrast.names = c(contrast.names, paste(contr[[i]][[1]], 
            "vs", contr[[i]][[2]], sep = " "))
        p.values = c(p.values, res$p.value)
        t.values = c(t.values, res$statistic)
        df = c(df, res$parameter)
        mean.1 = c(mean.1, mean(dat[dat$newfactor \%in\% contr[[i]][[1]], 
            dep]))
        mean.2 = c(mean.2, mean(dat[dat$newfactor \%in\% contr[[i]][[2]], 
            dep]))
    }
    p.values.corr = round(p.adjust(p.values, p.adjust.methods), 
        3)
    results = data.frame(contr = contrast.names, p.value.corr = p.values.corr, 
        t.value = t.values, df = df, mean.1 = mean.1, mean.2)
    attr(results, "p.corr") = p.adjust.methods
    cat("p values adjustment = ", p.adjust.methods, "\n")
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
