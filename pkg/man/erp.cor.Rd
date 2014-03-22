\name{erp.cor}
\alias{erp.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the timepoint correlations between ERPs (single electrode) and an external variable 
}
\description{
This function performs a series of correlations on each timepoint between ERP voltages and another (typically a behavioral) variable. The set of ERP data frames is specified by \code{base} and \code{numbers}. The behavioral variable is specified as a vector (of length equal to \code{numbers}) as the argument \code{behaviour}. The function returns a plot of a single electrode showing the results of the correlation tests.
}
\usage{
	erp.cor(base, numbers, electrode, behaviour=NULL, alpha=0.05, 
	method = c("pearson", "kendall", "spearman"), sig=NULL, 
	envir=.GlobalEnv, main=electrode, smo=0.5 , col="black", 
	startmsec=-200, endmsec=1200, interval=c(startmsec, endmsec), 
	step=200, verticals=NULL,horizontals=NULL, x.axis="default", 
	ylim=c(-6,6), lwd=c(1), lty=c(1), cex.xaxis=0.8, cex.yaxis=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base}{
a string indicating the beginning of the name of the set of ERP data frames.
}
  \item{numbers}{
the numbers of the subjects of the set of ERP data frames.
}
  \item{electrode}{
the electrode to be plotted.
}
  \item{behaviour}{
a vector indicating the values of the behavioural variables. This vector must be of the same length of \code{numbers}
}
  \item{alpha}{
the alpha of the correlation tests.
}
  \item{method}{
the method of the correlations. \cr
Available methods are \code{"pearson", "kendall", "spearman"}.
}
  \item{sig}{
if \code{NULL}, then a series of correlations is computed. If an appropriate object is specified (i.e. an object returned by \code{scalp.cor} or \code{erp.cor}) then the results are retrieved by this object and the function simply plots the results.
}
  \item{envir}{
the environment in which the ERP data frames are searched for.
}
 \item{main}{
plot title.
}

  \item{smo}{
the smoothing parameter to apply before plotting. Default is 0.
}
\item{col}{
the color of the waveform.
}

  \item{startmsec}{
the start time (in ms) of the ERP vector.
}
  \item{endmsec}{
the end time (in ms) of the ERP vector.
}
  \item{interval}{
a vector with a pair of values, indicating the interval to be plotted.
}
  \item{step}{
the step of the x-axis labels. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
the x-coordinates of the optional vertical bars.
}
  \item{horizontals}{
the y coordinates of the optional horizontal bars.
}
  \item{x.axis}{
If \code{default}, then an x.axis will be drawn on the basis of \code{interval} and \code{step} parameters. A vector of values can be supplied instead.
}
  \item{ylim}{
a pair of values indicating the y-axis limit.
}
  \item{lwd}{
line width.
}
  \item{lty}{
line type.
}

  \item{cex.xaxis}{
magnification factor of the x-axis labels.
}
  \item{cex.yaxis}{
magnification factor of the y-axis labels.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
This function plots a scalp map of the ERP waveforms in an array resembling the topographic locations of electrodes on the scalp.
The waveforms represent the average of the first set of data frames specified. Timepoints with a positive (and significant) correlation are highlighted by red bars. Timepoints with a negative (and significant) correlation are highlighted by blue bars. The function also returns an object with the correlation results. This object can be used in future plots with \code{erp.cor} or \code{scalp.cor}, by specifying it as the argument \code{sig}.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{scalp.cor}}
}
\examples{
	
\dontrun{
data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20) 
#generate some simulated RT data 
RT=rnorm(20, mean = 500, sd = 100)
erp.cor("Exp1_word_subj", 1:20, RT, Electrode="Fp1", ylim=c(-6,6))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
