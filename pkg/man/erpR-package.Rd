\name{erpR-package}
\alias{erpR-package}
\alias{erpR}
\docType{package}
\title{
erpR package
}
\description{
The erpR package includes a series of functions for importing Event-related potential (ERP) data, carrying out exploratory analyses, computing traditional ERP measures and ERP plotting.
}
\details{
\tabular{ll}{
Package: \tab erpR\cr
Type: \tab Package\cr
Version: \tab 0.2.0\cr
Date: \tab 2014-05-13\cr
License: \tab CC BY-NC-SA 3.0 \cr
}
The \code{erpR} package is dedicated to the analysis of event-related potentials (ERPs). Event-related potentials are the measured brain responses associated with a specific sensory, cognitive, or motor event and are obtained from electroencephalographic (EEG) signal. The \code{erpR} package contains a series of functions for importing Event-related potentials (ERP) data, carrying out exploratory analyses, computing traditional ERP measures and ERP plotting. Many \code{erpR} functions require to specify two arguments: \code{base} and \code{numbers}. The names of the objects for the \code{erpR} functions are obtained from the combination of \code{base} and \code{numbers}. The argument \code{base} typically indicates the name of the experimental condition (or group, or both), while \code{numbers} indicate the numbers of the subjects on which function is applied. This system is crucial to keep order in objects and files names and to tell \code{erpR} the correspondence between the objects. In particular a given \code{base} will always refer to the same condition, and a given \code{number} will always refer to the same subject. The data frames containing ERP data (named by the combination of \code{base} and \code{number}) are stored in lists called erplists.
This package is released under a non FOSS license since it relies on the package \code{\link{akima}}, that cannot be freely used for commercial uses.

Further information can be found on the maintainer's site \url{https://sites.google.com/site/giorgioarcara/erpr}.
}
\author{
	Giorgio Acara \email{giorgio.arcara@gmail.com}
	
	Anna Petrova \email{petrova@front.ru}

	Mantainer: Giorgio Arcara \email{giorgio.arcara@gmail.com}
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{

library(erpR)

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets) 


erp(word$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-10,10))


}
