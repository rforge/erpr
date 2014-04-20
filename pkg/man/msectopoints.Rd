\name{msectopoints}
\alias{msectopoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
convert points in milliseconds
}
\description{
An internal function of erpR to convert milliseconds in points.
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
     the time (in ms) of the start of the segment
}
  \item{endmsec}{
     the time (in ms) of the end of the segment
}
}
\details{
  This is mostly an internal function of \code{erpR}.
}
\value{
The function returns a single value that is the conversion of \code{a} from msec to points.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pointstomsec}}
}

\examples{
# convert 400 ms to points in a segment of 500 points starting from -200 ms 
# and ending at 1500 (500 Hz).

msectopoints(120, 500, startmsec=-500, endmsec=1500)

}					
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~miao }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
