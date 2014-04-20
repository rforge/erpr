\name{cn}
\alias{cn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
display column names
}
\description{
Return the column names of a \code{data.frame} or a \code{matrix} in one-column format for an easy visual inspection.
}
\usage{
cn(x)
}
\arguments{
  \item{x}{
a data.frame or a matrix with column names.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A one-column matrix with the column names of \code{x}.
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
