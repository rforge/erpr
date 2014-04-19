\name{erp.add}
\alias{erp.add}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
add an ERP waveform to a plot
}
\description{
Add a waveform to a ERP plot created with \code{erp}, \code{erp.t}, or \code{butterfly} functions.
}
\usage{
erp.add(el, startmsec=-200, endmsec=1200, interval=c(startmsec, endmsec), 
smo = NULL, col = "black", lty = 1, lwd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{el}{
the electrode to be plotted. 
}
\item{startmsec}{
the start time (in ms) of the ERP vector.
}
\item{endmsec}{
the end time (in ms) of the ERP vector.
}

  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}
  \item{interval}{
a vector with a pair of values indicating the interval (in milliseconds) to be plotted.
}


  \item{col}{
the color of the waveform.
}

  \item{lty}{
line type of the waveform .
}

  \item{lwd}{
line width of the waveform.
}
\item{...}{
	further parameters passed to \code{lines}
}
}

\value{
The specified waveform is added to an existing plot.
}

\author{
Giorgio Arcara
}


\seealso{
\code{\link{erp}}, \code{\link{butterfly}}, \code{\link{erp.t}}, \code{\link{erp.infl}}

}
\examples{

\dontrun{
data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20) 
nonword=grandaverage("Exp1_nonword_subj", 1:20) 

erp(word$Fp1, col="blue", startmsec=-200, endmsec=1500)
erp.add(nonword$Fp1, col="red")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
