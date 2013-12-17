\name{comp.add}
\alias{comp.add}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
comp.add
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
comp.add(en, lty = 1, smo = 0.5, col = "black", lwd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{en}{
%%     ~~Describe \code{en} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{smo}{
%%     ~~Describe \code{smo} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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
function (en, lty = 1, smo = 0.5, col = "black", lwd = 1) 
{
    en = smooth.spline(en, spar = smo)
    lines(en, col = col, lwd = lwd, lty = lty)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
