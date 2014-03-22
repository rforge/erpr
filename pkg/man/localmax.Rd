\name{localmax}
\alias{localmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate a local maximum
}
\description{
The function calculates a local maximum of the vector \code{x} surrounded by at least \code{n.points} lower numbers.
}
\usage{
localmax(x, n.points=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of numbers.
}
  \item{n.points}{
a minimum number of lower values to determine the local maximum. Default is two points.
}
}
\details{
The function calculates a local maximum of the vector \code{x}, i.e. the highest value of the vector \code{x} surrounded by at least \code{n} lower numbers. The concept of the local maximum applied to ERP analysis is taken from Luck (2005).
}
\value{
The local maximum of the vector \code{x} is returned.
}
\references{
Luck, S., J. (2005). \cite{An Introduction to the Event-Related Potential Technique.}
Cambridge, Mass.: The MIT Press.}
\author{
Giorgio Arcara
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
x=seq(-5.5,1,0.001)
y=sin(x)+0.3*x
plot(x,y, type="l")
abline(h=localmax(y)) #print an horizontal line intersecting the local maximum
}
}
