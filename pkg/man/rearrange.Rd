\name{rearrange}
\alias{rearrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rearrange
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rearrange(deps, oth = NULL, dataset, name.dep = "Dep", name.newvar = "New_Var")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deps}{
%%     ~~Describe \code{deps} here~~
}
  \item{oth}{
%%     ~~Describe \code{oth} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{name.dep}{
%%     ~~Describe \code{name.dep} here~~
}
  \item{name.newvar}{
%%     ~~Describe \code{name.newvar} here~~
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
function (deps, oth = NULL, dataset, name.dep = "Dep", name.newvar = "New_Var") 
{
    dep = as.vector(as.matrix(dataset[, deps]))
    others = NULL
    if (length(oth) > 0) {
        for (i in 1:dim(dataset[, deps])[2]) {
            others = rbind(others, dataset[, oth])
        }
    }
    nuova.var = NULL
    for (i in 1:length(names(dataset[, deps]))) {
        nuova.var = c(nuova.var, rep(names(dataset[, deps])[i], 
            dim(dataset[, deps])[1]))
    }
    dat = as.data.frame(cbind(dep, others))
    names(dat)[1] = paste(name.dep)
    dat[, paste(name.newvar)] = as.factor(nuova.var)
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
