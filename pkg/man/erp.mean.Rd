\name{erp.mean}
\alias{erp.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the mean of several ERP data frames in a given time window
}
\description{
This function calculates the mean amplitude of several ERP data frames. It returns the results in wide format (each electrode in a separate column) or in long format which is more suitable for standard statistical analysis in R (e.g. ANOVA). \code{erp.mean} applies the function \code{mean} on several ERP data frames with timepoints in rows and electrodes in columns. ERP data frames are expected to be named as a combination of strings specified in \code{base} and \code{numbers}.  
}
\usage{
erp.mean(base, numbers, win.ini, win.end, erplist = NULL, 
startmsec=-200, endmsec=1200, others=NULL, format="long", 
name.dep="Dep", name.newvar="electrode")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of \code{data.frame} containing ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
  \item{win.ini}{
the initial edge (in ms) of the window on which the mean amplitude has to be computed.
}
  \item{win.end}{
the final edge (in ms) of the window on which the mean amplitude has to be computed.
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
	the name of the dependent variable in the returned \code{data.frame}. Ignored if format is "wide".
}
\item{name.newvar}{
	If the format is "long", the name of the new variable codifying the electrodes. Default is "electrode".
}
}

\details{
The function takes an ERP data frame with electrodes in separate columns as input. If the format is long, this function returns a data frame in long format ready for standard statistical analysis in R.  The returned data frame has one column for the dependent variable and a new factor "electrode". 
}
\value{
A ERP data frame with mean amplitude in long or wide format. Two more columns are added: the column \code{Subject} indicates the subject \code{number}; the column \code{Subject_name} reports the string retrieved from the comment of the data frame (see Details on \code{\link{import.erp}}). 
}

\author{
Giorgio Arcara
}
\note{
This function calls \code{\link{rearrange}}.
}


\seealso{
\code{\link{erp.peak}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{mean}} 

}
\examples{

data(ERPsets)

dat=erp.mean(base="Exp1_word_subj", numbers=1:20, 
win.ini=130, win.end=190, erplist=ERPsets, startmsec=-200, endmsec=1500,
others=c(condition="word", interval="130-190"), name.dep="Ampl", format="long")


}

