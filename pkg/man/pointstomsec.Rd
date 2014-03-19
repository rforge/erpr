\name{pointstomsec}
\alias{pointstomsec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pointstomsec
}
\description{
%% internal function of erpR to convert points in msec
}
\usage{
pointstomsec(a, lengthsegment, startmsec, endmsec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
   a number indicating the point to be converted in ms
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
The function return a single value that is the conversion of \code{a} from points to msec.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Giorgio arcara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{msectopoints}}
}
\examples{

# calculate the time in msec corrsponding to the point n 128 in a segment of 500 points starting from -200 ms and ending at 1500 (500 Hz).

pointstomsec(128, 500, startmsec = - 500, endmsec = 1500)
}					
