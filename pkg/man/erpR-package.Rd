\name{erpR-package}
\alias{erpR-package}
\alias{erpR}
\docType{package}
\title{
erpR-package
}
\description{
The erpR package includes a series of functions for computing traditional ERP measures, plotting, importing ERP data and carrying out exploratory analyses.
}
\details{
\tabular{ll}{
Package: \tab erpR\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-03-19\cr
License: \tab GNU public license \cr
}
The package includes a series of functions for computing traditional ERP measures, plotting, importing ERP data and carrying out exploratory analyses. 
In many \code{erpR} functions it will be required to specify the two arguments \code{base} and \code{numbers}. From the combination of \code{base}  and \code{numbers}, it is obtained the name of the objects (or of the files) on which the functions of \code{erpR} will be applied. The argument \code{base} typically indicates the name of the experimental condition (or group or both), whereas \code{numbers} typically indicate the subject numbers on which apply the function. This system is crucial to keep order in object and files names and to tell \code{erpR} the correspondence between objects of files. In particular a given \code{base} will always refer to the same condition, and a given \code{number} will always refer to the same subject.

Further information can be found on the maintainer's site \url{https://sites.google.com/site/giorgioarcara/}.
}
\author{
	Giorgio Acara \email{giorgio.arcara@gmail.com}
	
	Anna Petrova

	Mantainer: giorgio.arcara@gmail.com
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
\dontrun{
library(erpR)
data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20) 
erp(word$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-20,20))
}
}
