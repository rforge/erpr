\name{factorall}
\alias{factorall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
factorall
}
\description{
Convenience function to update all factor variables in a \code{data.frame}.
}
\usage{
factorall(x)
}
\arguments{
  \item{x}{
The \code{data.frame} to be updated.
}
}

\value{
A \code{data.frame} in which all factors have been updated. All unused levels are dropped.
}

\author{
Giorgio Arcara.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(iris)

iris2 <- iris[iris$Species!="setosa", ]
levels(iris2$Species)

iris2<-factorall(iris2)
levels(iris2$Species)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
