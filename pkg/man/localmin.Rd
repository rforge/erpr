\name{localmin}
\alias{localmin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate local minimum
}
\description{
The function calculates a local minimum of the vector \code{x} surrounded by at least \code{n.points} higher numbers.

}
\usage{
localmin(x, n.points = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of numbers.
}
  \item{n.points}{
the minimum numbers of higher values to determine the local minimum (see Details). Default is two points.
}
}
\details{
The function calculates the local minimum of the vector \code{x}, i.e. the lowest value of the vector \code{x} surrounded by at least \code{n} higher numbers. The concept of local minimum applied to ERP analysis is taken from Luck (2005).
}
\value{
The local minimum of the vector \code{x} is returned.
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
y=-(sin(x)+0.3*x)
plot(x,y, type="l")
abline(h=localmin(y)) #print an horizontal line intersecting the local minimum
}
}
