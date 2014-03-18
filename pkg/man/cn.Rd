\name{cn}
\alias{cn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cn
}
\description{
return column names of a \code{data.frame} or \code{matrix} in one-column format for a quick inspection of column names.
}
\usage{
cn(x)
}
\arguments{
  \item{x}{
\code{x} is a data.frame or a matrix with column names
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A one-column matrix with the column names of \code{x}
}
\author{
Giorgio Arcara
}
\note{
This function comes from an idea by Antoine Tremblay.
}


\examples{
data(iris)
cn(iris)
}
