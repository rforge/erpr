\name{sampling.rate}
\alias{sampling.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate sampling rate from an \code{erpobj}
}
\description{
The function calculates calculate sampling rate from an \code{erpobj}, that is a \code{data.frame} containing ERP data used with the \code{erpR} package. 
}
\usage{
sampling.rate(x, startmsec = NULL, endmsec = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An \code{erpobj}, that is a \code{data.frame} containing ERP data used with the \code{erpR} package.  
}
  \item{startmsec}{
the time (in ms) of the start of the segment
}
  \item{endmsec}{
the time (in ms) of the end of the segment
  }
}
\details{
This function can be used to calculate the sampling rate of an \code{erpobj}.
}
\value{
The sampling rate.
}

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

# load sample data
data(erplistExample)

# calculate sampling rate of the first object in the erplist, named erplistExample
sampling.rate(erplistExample[[1]], startmsec = -200, endmsec = 1500)

}
