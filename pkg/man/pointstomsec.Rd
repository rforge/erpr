\name{pointstomsec}
\alias{pointstomsec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pointstomsec
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pointstomsec(a, lengthsegment, startmsec, endmsec)
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

function(a, lengthsegment, startmsec, endmsec){
			x=(((endmsec-(startmsec))*(a-1))/(lengthsegment-1))
			return(x)
			}
			
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
