\name{erp.infl}
\alias{erp.infl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
inspect the influence of a subject on the average (single electrode).
}
\description{
This function returns a diagnostic plot to investigate the influence of a given ERP data frame on the grandaverage of a series of ERP data frames. A GUI is provided to explore the effect of removing a data frame (that is expected to be associated with a Subject) on the grandaverage. Two plots are superposed: the original grandaverage and the average removing the subject selected via the GUI.
}

\usage{ erp.infl(base, numbers, electrode, smo=0, startmsec=-200, 
	endmsec=1200, lwd=1, lty=1, envir=.GlobalEnv, ...)}

\arguments{
\item{base}{
a string indicating the beginning of the name of the \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}

  \item{electrode}{
the electrode to be plotted.
}
  \item{smo}{
the smoothing parameter to apply before plotting. Default is 0.
}

  \item{startmsec}{
the start time (in ms) of the ERP vector.
}
  \item{endmsec}{
the end time (in ms) of the ERP vector.
}
  \item{lwd}{
line width.
}
  
  \item{lty}{
line type.
}
  \item{envir}{
the environment in which \code{erp.infl} is to be evaluated.
}
\item{\ldots}{
further parameters to be passed to \code{erp} function called within \code{erp.infl}.
}

}

\details{
The function requires the package \code{rpanel}
}

\author{
Giorgio Arcara
}



\seealso{
\code{\link{erp}}, \code{link{scalp.infl}}
}
\examples{
\dontrun{

data(ERPsets)
erp.infl(base="Exp1_word_subj", numbers=1:20, electrode="OZ", startmsec=-200, endmsec=1500)
# Notice that Subject 1 is clearly particularly influential for the average on OZ.

}

}

