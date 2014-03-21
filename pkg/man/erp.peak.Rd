\name{erp.peak}
\alias{erp.peak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate peak of several ERP data frames on a given time window
}
\description{
This function calculates the peak amplitude, on several ERP \code{data.frame}. It returns results in "wide" format (each electrode in a column) or in "long" format, a \code{data.frame}format more suitable for standard statistical analysis in R (e.g. ANOVA). \code{erp.peak} applies the function \code{mean} on several ERP data frames with timepoints in rows and electrodes in columns. ERP data frames are expected to be named as a combination of strings specified in \code{base} and \code{numbers}.
}
\usage{
erp.peak(base, numbers, win.ini, win.end, envir=.GlobalEnv, startmsec=-200, endmsec=1200, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", peak.fun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
  \item{win.ini}{
a value indicating the initial edge (in ms) of the window on which the peak amplitude has to be computed.
}
  \item{win.end}{
a value indicating the final edge (in ms) of the window on which the peak amplitude has to be computed.
}
  \item{envir}{
The environment in which \code{erp.peak} is to be evaluated.
}
  \item{startmsec}{
a value indicating the start time (in ms) of the ERP data frames. It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
a value indicating the final time (in ms) of the ERP data frames.
}
  \item{others}{
the names of other variables to be included in the returned \code{data.frame}. Ignored if format is "wide".
}
\item{format}{
	the output format. It may be "wide" or "long".
}
\item{name.dep}{
	the name of the dependent variable in the returned \code{data.frame}. Ignored if format is "wide".
}
\item{peak.fun}{
	the function to be used to calculate the peak. Available functions are \code{max}, \code{min}, \code{localmax}, \code{localmin}.
	}
\item{name.newvar}{
	If format =="long", the name of the new variable codifying the electrodes. Default value is "Electrode".
}
\item{...}{
	arguments to be passed to the function specified in \code{peak.fun}.
	}
}

\details{
The function takes as input an ERP \code{data.frame} with electrodes in separate columns. If the format is "long", this function returns a \code{data.frame} in long format ready for standard statistical analysis in R.  The returned \code{data.frame} has one column for dependent variable and a new factor "Electrode". \code{localmax} and \code{localmin} are two \code{erpR} functions written following Luck (2005).
}
\value{
An ERP \code{data.frame} with peak amplitude in long or wide format. 
}
\references{
Luck, S., J. (2005). \cite{An Introduction to the Event-Related Potential Technique.}
Cambridge, Mass.: The MIT Press.
}

\author{
Giorgio Arcara
}
\note{
this function calls \code{\link{rearrange}}.
}


\seealso{
\code{\link{erp.mean}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{max}},   \code{\link{min}},  \code{\link{localmax}},  \code{\link{localmin}}
}
\examples{
\dontrun{
data(ERPsets)
erp.peak(base="Exp1_word_subj", numbers=1:20, 
win.ini=130, win.end=190, startmsec=-200, endmsec=1500,
others=c(condition="word", interval="130-190"), name.dep="Ampl", format="long", peak.fun=max)
}
}