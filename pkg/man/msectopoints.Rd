\name{msectopoints}
\alias{msectopoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
msectopoints
}
\description{
%% internal function of erpR to convert points in msec
}
\usage{
msectopoints(a, lengthsegment, startmsec, endmsec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
   a number indicating the ms to be converted in points
}
  \item{lengthsegment}{
    the length (in points) of the segment
}
  \item{startmsec}{
     the time (in ms) of the beginning of the segment
}
  \item{endmsec}{
     the time (in ms) of the end of the segment
}
}
\details{
  this function is mostly an internal function.
}
\value{
The function return a single number \eqn{x}, obtained by the conversion given by \deqn{x = \frac{((a-(startmsec))*(lengthsegment-1))}{(endmsec-(startmsec))}}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Giorgio arcara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pointstomsec}}
}

\examples{

# convert 400 ms to points in a segment of 500 points starting from -200 ms and ending at 1500 (500 Hz).

msectopoints(120, 500, startmsec=-500, endmsec=1500)
}					
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~miao }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
