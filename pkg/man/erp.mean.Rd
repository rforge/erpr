\name{erp.mean}
\alias{erp.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate mean of several ERP data frames on a given time window
}
\description{
This function calculates the mean amplitude on several ERP data frames. It returns results in "wide" format (each electrode in a column) or in "long" format, a \code{data.frame} format more suitable for standard statistical analysis in R (e.g. ANOVA). \code{erp.mean} applies the function \code{mean} on several ERP data.frames with timepoints in rows and electrodes in columns. ERP data frames are expected to be named as a combination of strings specified in \code{base} and \code{numbers}.  
}
\usage{
erp.mean(base, numbers, win.ini, win.end, envir=.GlobalEnv, startmsec=-200, endmsec=1200, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of \code{data.frame} containing the ERP data.
}
  \item{numbers}{
The numbers of subjects to be averaged by the function.
}
  \item{win.ini}{
a value indicating the initial edge (in ms) of the window on which the mean amplitude has to be computed.
}
  \item{win.end}{
a value indicating the final edge (in ms) of the window on which the mean amplitude has to be computed.
}
  \item{envir}{
the environment in which \code{erp.mean} is to be evaluated.
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
\item{name.newvar}{
	If format =="long", the name of the new variable codifying the electrodes. Default value is "Electrode".
}
}

\details{
The function takes as input an ERP data frame with electrodes in separate columns. If the format is "long", this function returns a \code{data.frame} in long format ready for standard statistical analysis in R.  The returned \code{data.frame} has one column for the dependent variable and a new factor "Electrode".
}
\value{
a ERP \code{data.frame} with mean amplitude in long or wide format. 
}

\author{
Giorgio Arcara
}
\note{
this function calls \code{\link{rearrange}}.
}


\seealso{
\code{\link{erp.peak}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{mean}} 

}
\examples{
\dontrun{
data(ERPsets)
erp.mean(base="Exp1_word_subj", numbers=1:20, 
win.ini=130, win.end=190, startmsec=-200, endmsec=1500,
others=c(condition="word", interval="130-190"), name.dep="Ampl", format="long")

}
}

