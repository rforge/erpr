\name{stable.diff.fun}
\alias{stable.diff.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate stable difference in timepoints in ERP waveforms.
}
\description{
An internal function to perform a series of t-tests (one for each timepoint) and filter for stable differences.
}
\usage{
stable.diff.fun(res.log.mat, electrodes, crit.npoints, interval, startmsec, endmsec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res.log.mat}{
A logical matrix, results of mass.t.test}
  \item{electrodes}{
the electrodes labels.
}

\item{crit.npoints}{
Only sequences of consecutive significant timpoints during at least \code{crit.npoints} number of timepoints will be retained in the results.

}

  \item{interval}{
a vector with a pair of values indicating the interval (in milliseconds) to be plotted.
}
\item{startmsec}{
the start time (in ms) of the ERP vector.
}
\item{endmsec}{
the end time (in ms) of the ERP vector.
}

}
\details{
This is an internal function called by \code{mass.t.test}. This function filters the results of a matrix with results, output of \code{mass.t.test}), excluding those significant results that not respect the requirements specified in \code{crit.npoints}. The number in \code{crit.npoints} specify how many consecutive significant results (in the same electrode), should occur to retain the signficant result. For example if crit.npoints=10, only sequence of at least 10 consecutively significant timepoints will be retained. 
}
\value{
A \code{data.frame} with the results of the t-tests specified.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mass.t.test}}
}
\examples{

#########################
# INTERNAL FUNCTION (NO EXAMPLES)
#########################

}
