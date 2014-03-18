\name{average.infl}
\alias{average.infl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average.infl
}
\description{
The function returns a diagnostic plot to investigate the influence of a given ERP \code{data.frame} on the grandaverage of a series of ERP \code{data.frame}. A GUI is provided to explore the effect of removing a \code{data.frame} (that is expected to be associated with a Subject) on the grandaverage. Two plots are superposed: the original grandaverage, and the average removing the subject selected via the GUI.
}

\usage{ average.infl(base, numbers, electrode, smo=0, startmsec=-200, endmsec=1200, lwd=1, lty=1, env=.GlobalEnv, ...)}

\arguments{
\item{base}{
A string indicating the beginning of the name of \code{data.frame} containg the ERP data.
}
  \item{numbers}{
The numbers (indicating different subjects), to be averaged by the function.
}

  \item{electrode}{
The electrode to be plotted.
}
  \item{smo}{
The smoothing parameter to be applied to the data.
}

  \item{startmsec}{
A value, in milliseconds indicating the beginning time of the ERP vector
}
  \item{endmsec}{
A value, in milliseconds indicating the end time of the ERP vector
}
  \item{lwd}{
The width of the ERP lines to be plotted.
}
  \item{env}{
The environment in which \code{grandaverage} is to be evaluated.
}
\item{\ldots}{
Further parameters to be passed to \code{erp} function called within \code{average.infl}.
}

}

\details{
The function requires the package \code{\link{rpanel}}
}

\author{
Giorgio Arcara
}



\seealso{
\code{\link{erp}}, \code{link{scalp.infl}}
}
\examples{
\dontrun{

# Note that Subject 1 is clearly particularly influential for the average on OZ.
average.infl(base="Exp1_word_subj", numbers=1:20, electrode="OZ", startmsec=-200, endmsec=1500)

}

}

