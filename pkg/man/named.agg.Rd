\name{named.agg}
\alias{named.agg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
aggregate and keep the original names
}
\description{
A wrapper of the \code{aggregate} function. Data are provided as a \code{formula} and the original names are kept in the returned \code{data.frame}.
}
\usage{
named.agg(formula, data, FUN, ..., subset, na.action = na.omit) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a \code{formula}, such as \code{y ~ x }or \code{cbind(y1, y2) ~ x1 + x2}, where the \code{y} variables are numeric data to be split into groups according to the grouping x variables (typically factors).
}
  \item{data}{
the \code{data.frame} to be aggregated.
}
  \item{FUN}{
a function to compute the summary statistics which can be applied to all data subsets.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
	\item{na.action}{
	a function which indicates what should happen when the data contain \code{NA} values. The default is to ignore missing values in the given variables.
}
\item{...}{
		further arguments passed to or used by methods.
}

}

\value{
For the data frame method, teh function returns a data frame with columns corresponding to the grouping variables in the right part of the formula, followed by aggregated columns from the left part of the formula. The names of the variables remain the same as in the original data frame. 
}

\author{
Ananda Mahto
}

\note{
named.agg is a wrapper of \code{\link{aggregate}}
}

\seealso{
\code{\link{aggregate}}
}
\examples{
\dontrun{
data(ERPsets)

datall=erp.mean(base = "Exp1_word_subj", numbers = 1:20, 
win.ini = 400, win.end = 600, startmsec= -200, endmsec = 1500, format="long", name.dep="Ampl")

datagg=named.agg(Ampl~electrode+Subject, datall, FUN=mean)
}
}
