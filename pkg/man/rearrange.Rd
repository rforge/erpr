\name{rearrange}
\alias{rearrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rearrange a data frame from wide to long format
}
\description{
This utility function of the \code{erpR} package converts an ERP data frame with a \cite{single} variable in wide format to a long format. 
}
\usage{
rearrange(deps, oth = NULL, dataset, name.dep = "Dep", name.newvar = "New_Var")
}
\arguments{
  \item{deps}{
the numbers of the columns in the specified data frame to be rearranged from wide format to long format.
}
  \item{oth}{
other variables that are to be kept in the returned data frame.
}
  \item{dataset}{
the name of the data frame to be converted from wide format to long format.
}
  \item{name.dep}{
the name of the dependent variable in the returned data frame.
}
  \item{name.newvar}{
the name of the variable indicating the levels associated with \code{name.dep} in the returned data frame.
}
}
\details{
The function converts a dataframe  with a \cite{single} variable in wide format (levels are in separate columns) in a data frame in long format (the variable is in a single column and the levels are specified by other columns). The original variable in wide format is represented in two columns: one (labeled by \code{name.dep} parameter) indicating the values of the dependent variable, and the other that is a factor (labeled by \code{name.newvar} parameter) indicating the levels of the variable for each observation. The variables specified in \code{oth} are replicated appropriately. This function is called internally by \code{erp.mean}, \code{erp.peak}, and \code{erp.latency}.
}
\value{
A data frame in which the variables specified in the columns \code{deps} are converted in a single column \code{name.dep} with levels specified in \code{name.newvar}
}
\author{
Giorgio Arcara
}
\note{
This function is called internally by \code{erp.mean}, \code{erp.peak}, and \code{erp.latency}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp.mean}}, \code{\link{erp.peak}}, \code{\link{erp.latency}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(erplistExample)

datwide=erp.mean(base="Exp1_word_subj", numbers=1:20, win.ini=130, 
win.end=190, erplist=erplistExample, startmsec=-200, endmsec=1500, format="wide")

dat.long=rearrange(deps=c(1:32), oth=33:34, dataset=datwide, name.newvar="electrode")


}
