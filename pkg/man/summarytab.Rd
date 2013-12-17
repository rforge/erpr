\name{summarytab}
\alias{summarytab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summarytab
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarytab(deps, groups, na.rm = TRUE, substr.l = max(nchar(levels(groups))))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deps}{
%%     ~~Describe \code{deps} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{substr.l}{
%%     ~~Describe \code{substr.l} here~~
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
function (deps, groups, na.rm = TRUE, substr.l = max(nchar(levels(groups)))) 
{
    deps = as.data.frame(deps)
    groups = apply(as.data.frame(groups), 1, function(x) {
        paste(x, collapse = "_")
    })
    groups = as.factor(groups)
    levels(groups) = substr(levels(groups), 1, substr.l)
    for (i in 1:length(deps)) {
        sum.Ntot = as.numeric(table(groups))
        sum.Nvalid = as.numeric(table(groups[!is.na(deps[, i])]))
        sum.NA = as.numeric(table(groups[is.na(deps[, i])]))
        sum.mean = tapply(deps[, i], groups, mean, na.rm = na.rm)
        sum.sd = tapply(deps[, i], groups, sd, na.rm = na.rm)
        sum.max = tapply(deps[, i], groups, max, na.rm = na.rm)
        sum.min = tapply(deps[, i], groups, min, na.rm = na.rm)
        results = data.frame(MEAN = as.numeric(sum.mean), SD = as.numeric(sum.sd), 
            MIN = as.numeric(sum.min), MAX = as.numeric(sum.max), 
            Ntot = sum.Ntot, Nvalid = sum.Nvalid, NotAv = sum.NA)
        rownames(results) = levels(groups)
        cat("--------------------------\n")
        cat("--", names(deps)[i], "--\n")
        print(results)
        cat("--------------------------\n")
        cat("\n\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
