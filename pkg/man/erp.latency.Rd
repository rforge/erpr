\name{erp.latency}
\alias{erp.latency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate latency of several ERP data frames on a given time window
}
\description{
This function calculates the peak latency on several ERP data frames. It returns the results in wide format (each electrode in a separate column) or in long format which is more suitable for standard statistical analysis in R (e.g. ANOVA). \code{erp.latency} applies the function \code{mean} to several ERP data frames with timepoints in rows and electrodes in columns. ERP data frames are expected to be named as a combination of strings specified in \code{base} and \code{numbers}.
}
\usage{
erp.latency(base, numbers, win.ini, win.end, erplist = NULL, 
startmsec=NULL, endmsec=NULL, others=NULL, format="long", 
name.dep="Dep", name.newvar="electrode", peak.fun = "max", frac=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of the \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
  \item{win.ini}{
the initial edge (in ms) of the window in which the peak latency has to be computed.
}
  \item{win.end}{
the final edge (in ms) of the window in which the peak latency has to be computed.
}
  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}
  \item{startmsec}{
the start time (in ms) of the ERP data frames. It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
the end time (in ms) of the ERP data frames.
}
  \item{others}{
the names of other variables to be included in the returned data frame. Ignored if the format is "wide".
}
\item{format}{
	the output format. It may be "wide" or "long".
}
\item{name.dep}{
	the name of the dependent variable in the returned dataframe. Ignored if the format is "wide".
}

\item{name.newvar}{
	If the format is "long", the name of the new variable codifying the electrodes. Default is "electrode".
}
\item{peak.fun}{
	the function to calculate the peak. Available functions are \code{max}, \code{min}, \code{localmax}, \code{localmin}.
	}
\item{frac}{
To measure the "Fractional Peak Latency", this argument is number indicating the fraction of the peak maximum to be used to localize the peak. Default is 1, and the maximum (or minimum) peak is searched. If 0.5, the first peak before the maximum/mimimum, with 0.5 the size the maximum/minimum is searched.
	}
\item{...}{
	arguments passed to the function specified in \code{peak.fun}.
	}


}

\details{
The function takes an ERP data frame with electrodes in separate columns as input. If the format is long, this function returns a data frame in long format ready for standard statistical analysis in R.  The returned data frame has one column for the dependent variable and a new factor "electrode". The \code{erpR} functions \code{localmax} and \code{localmin} are written following Luck (2005).
}
\value{
A ERP data frame with peak latency in long or wide format. Two more columns are added: the column \code{Subject} indicates the subject \code{number}; the column \code{Subject_name} reports the string retrieved from the comment of the data frame (see Details on \code{\link{import.erp}}). 
}
\references{
Luck, S., J. (2005). \cite{An Introduction to the Event-Related Potential Technique.}
Cambridge, Mass.: The MIT Press.
}

\author{
Giorgio Arcara
}
\note{
This function calls \code{\link{rearrange}}.
}


\seealso{
\code{\link{erp.mean}}, \code{\link{erp.peak}}, \code{\link{rearrange}},  \code{\link{max}},   \code{\link{min}},  \code{\link{localmax}},  \code{\link{localmin}}
}
\examples{
data(erplistExample)

dat=erp.latency(base="Exp1_word_subj", numbers=1:20,
 win.ini=130, win.end=190, erplist=erplistExample,
others=c(condition="word", interval="130-190"), 
name.dep="Ampl", format="long", peak.fun=max)


}
