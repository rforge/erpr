\name{factorall}
\alias{factorall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Factorize more then one variable in a data frame
}
\description{
A convenience function to transform on or more variable of a data frame into factor.
}
\usage{
factorall(x)
}
\arguments{
  \item{x}{
one or more data frame columns to be transformed into factor.
}
}

\value{
The vectors supplied are transformed into factors. Notice that if a variable is already a factor, it is updated and all unused levels are dropped.
}

\author{
Giorgio Arcara.
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
data(iris)

iris2 <- iris[iris$Species!="setosa", ]
levels(iris2$Species)

iris2[,c("Petal.Width", "Species")]<-factorall(iris2[,c("Petal.Width", "Species")])
levels(iris2$Species)
str(iris2)
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
