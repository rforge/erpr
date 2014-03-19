\name{char2fac}
\alias{char2fac}

\title{
char2fac
}
\description{
convert each \code{character} column in a \code{data.frame} to a \code{factor}. 
}
\usage{
char2fac(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the \code{data.frame} in which columns are to be coverted.
}
}

\value{
A \code{data.frame} with \code{character} columns converted in \code{factor} columns.
}

\author{
Giorgio Arcara.
}
\note{
This is an utility function to quickly transform \code{character} vector as \code{factor} for analysis purpose.
}



\examples{
	
	dat=data.frame(RT=rnorm(20, mean=500, sd=100), Subject=rep(1:10, 2))
	conditions=c(rep("a", 10), rep("b", 10))
	
	dat$conditions=conditions
	
	dat=char2fac(dat)

}
