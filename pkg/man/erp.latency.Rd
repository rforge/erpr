\name{erp.latency}
\alias{erp.latency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
erp.latency
}
\description{
This function calculates the peak latency on several ERP \code{data.frame}. It returns results in "wide" format (each electrode in a columen) or in "long" format, a \code{data.frame}format more suitable for standard statistical analysis in R (e.g. ANOVA). \code{erp.latency} applies the function \code{mean} on several ERP \code{data.frame} objects, i.e. data.frames with timepoints on rows and electrodes on columns. ERP data.frames are expected to be named as a combination of strings specified in \code{base} and \code{numbers}. Differently from \code{\link{erp.fun}} this function returns a data.frame in long format (see Details).  
}
\usage{
erp.latency(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, fun=average.mean, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", peak.fun, ...)
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
A value indicating the initial edge (in ms) of the windows on which the peak latency has to be computed.
}
  \item{win.end}{
A value indicating the final edge (in ms) of the windows on which the peak latency has to be computed.
}
  \item{env}{
The environment in which \code{erp.fun} is to be evaluated.
}
  \item{startmsec}{
A value indicating the initial time (in ms) of the ERP \code{data.frame} objects. It can be a negative value, indicating the baseline period.
}
  \item{endmsec}{
A value indicating the final time (in ms) of the ERP \code{data.frame} objects.
}
  \item{others}{
the name of others variables to be included in the returned \code{data.frame}. Ignored if format is "wide".
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
\item{...}{
	arguments to be passed to the function specified in \code{peak.fun}.
	}


}

\details{
The function takes as input ERP \code{data.frame} with electrodes in separate columns. If format == "long", this function returns a \code{data.frame} in long format ready for standard statistical analysis in R.  The returned \code{data.frame} has one column for dependent variable and a new factor "Electrode". \code{localmax} and \code{localmin} are two \code{erpR} functions written following Luck (2005).
}
\value{
A ERP \code{data.frame} with peak latency in long or wide format. 
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
\code{\link{erp.mean}}, \code{\link{erp.peak}}, \code{\link{rearrange}},  \code{\link{max}},   \code{\link{min}},  \code{\link{localmax}},  \code{\link{localmin}}
}
\examples{
	
erp.latency(base="Exp1_word_subj", numbers=1:20, win.ini=130, win.end=190, startmsec=-200, endmsec=1500,
		others=c(condition="word"], interval="130-190", name.dep="Ampl", format="long"), peak.fun=max)
}
