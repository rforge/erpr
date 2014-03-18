\name{rearrange}
\alias{rearrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rearrange
}
\description{
This utility function of the \code{erpR} package convert a ERP \code{data.frame} with a \cite{single} variable in wide format to a long format. 
}
\usage{
rearrange(deps, oth = NULL, dataset, name.dep = "Dep", name.newvar = "New_Var")
}
\arguments{
  \item{deps}{
The numbers indicating the columns in the data.frame specified to be rearranged from wide format to long format.
}
  \item{oth}{
Others variables that are to be kept in the returned \code{data.frame}.
}
  \item{dataset}{
The name of the \code{data.frame} to be converted from wide format to long format.
}
  \item{name.dep}{
The name of the dependent variable in the returned \code{data.frame}.
}
  \item{name.newvar}{
The name of the variable indicating the levels associated with \code{name.dep} in the returned \code{data.frame}.
}
}
\details{
The function convert a \code{data.frame} with a \cite{single} variable in wide format (levels are in different columns) in a \code{data.frame} in long format (the variable is in a single column and the levels are specified by another columns). The original variable in wide format will be represente in two columns: one (labeled by \code{name.dep} parameter) indicating the values of the dependent variable, and one that will be a factor (labeled by \code{name.newvar} parameter) indicating the levels of the variables for each observation. The variables specified in \code{oth} will be replicated appropriately. This function is called internally by \code{erp.measure}.
}
\value{
A data frame in which the variables specified in the columns \code{deps} are converted in a single column \code{name.dep} with levels specified in \code{name.newvar}
}

\author{
Giorgio Arcara
}
\note{
This function is called internally by \code{erp.measure}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp.measure}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dontrun{
### DA CREARE DATA.FRAME
dat.wide=ERPwide
dat.long=rearrange(deps=c(1:32), oth=33, dataset=dat.wide, name.newvar="Electrode")
}

}
