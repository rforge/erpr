\name{average.mean}
\alias{average.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average.mean
}
\description{
A function to calculate the mean amplitude of ERP \code{data.frame} objects, i.e. data.frames with timepoints on rows and electrodes on columns. ERP data.frames are expected to be named according to a combination of \code{base} and \code{numbers}.
}
\usage{
average.mean(base, numbers, win.ini, win.end, env = .GlobalEnv, startmsec = -200, endmsec = 1200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base}{
A string indicating the beginning of the name of \code{data.frame} containg the ERP data.
}
  \item{numbers}{
The numbers (indicating different subjects), to be averaged by the function.
}
  \item{win.ini}{
A value indicating the initial edge (in ms) of the windows on which the mean amplitude has to be computed.
}
  \item{win.end}{
A value indicating the final edge (in ms) of the windows on which the mean amplitude has to be computed.
}

  \item{env}{
The environment in which \code{average.mean} is to be evaluated.
}
  \item{startmsec}{
A value indicating the initial time (in ms) of the ERP \code{data.frame} objects. It can be a negative value, indicating the baseline period.
}
  \item{endmsec}{
A value indicating the final time (in ms) of the ERP \code{data.frame} objects.
}
}


\value{
A data.frame with a single row with the mean amplitude values for each electrodes for the time window specified by \code{win.ini} and \code{win.end}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Giorgio Arcara
}

\note{
This function is called by the \code{\link{erp.measure}} function.
	
}

\seealso{
\code{\link{average.peak}}, \code{\link{average.peak}}, \code{\link{average.mean.NA}}, \code{\link{erp.measure}}.

}
\examples{
\dontrun{
	average.mean("Exp1_word_subj", 1:20, win.ini=400, win.end=600, startmsec=-200, endmsec=1500)
	
}
}
