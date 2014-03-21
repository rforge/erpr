\name{erp.cor}
\alias{erp.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the timepoint correlations between ERPs and an external variable (single electrdode)
}
\description{
This function performs a series of correlations on each timepoint betwen ERP voltages and another (usually a behavioural) variable. The set of ERP data frames is specified by \code{base1} and \code{numbers}. The behavioral variable is specified as a vector (of length equal to \code{numbers}) as the argument \code{behaviour}. A plot of a single electrode showing the results of the correlation tests is returned.
}
\usage{
	erp.cor(base1, numbers, Electrode, behaviour=NULL, alpha=0.05, 
	method = c("pearson", "kendall", "spearman"), sig=NULL, 
	envir=.GlobalEnv, main=Electrode, smo=0.5 , col="black", 
	startmsec=-200, endmsec=1200, interval=c(startmsec, endmsec), 
	step=200, verticals=NULL,horizontals=NULL, x.axis="default", 
	ylim=c(-6,6), lwd=c(1), lty=c(1), cex.xaxis=0.8, cex.yaxis=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base1}{
A string indicating the beginning of the name of the set of ERP data frames.
}
  \item{numbers}{
The numbers (indicating different subjects), of the set of ERP data frames .
}
  \item{Electrode}{
The electrode to be plotted
}
  \item{behaviour}{
A vector indicating the values of the behavioural variables. This vector must be of the same length of \code{numbers}
}
  \item{alpha}{
The alpha of the correlation tests.
}
  \item{method}{
The method of the correlations. 
Available methods are \code{"pearson", "kendall", "spearman"}.
}
  \item{sig}{
If \code{NULL}, then the series of correlations is computed. If an appropriate object is specified (i.e. the object returned by \code{scalp.cor} or \code{erp.cor}) then the results are retrieved by this object and the function simply plots the results.
}
  \item{envir}{
The environment in which the ERP data frames  are searched.
}
 \item{main}{
Title of the plot.
}

  \item{smo}{
The smoothing parameter to be applied before plotting. The default is 0.
}
\item{col}{
Color of the waveform.
}

  \item{startmsec}{
A value, in milliseconds indicating the beginning time of the ERP vector
}
  \item{endmsec}{
A value, in milliseconds indicating the end time of the ERP vector
}
  \item{interval}{
A vector with a pair of values, indicating the interval to be plotted
}
  \item{step}{
The step on the x-axis labels to be plotted. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
The x coordinates of optional vertical bars to be plotted
}
  \item{horizontals}{
The y coordinates of optional vertical bars to be plotted
}
  \item{x.axis}{
If \code{default}, than an x.axis will be drawn on the bases of \code{interval} and \code{step} parameters. A vector of values can be supplied instead.
}
  \item{ylim}{
A pair of values indicating the y-axis limit.
}
  \item{lwd}{
The width of the ERP line to be plotted.
}
  \item{lty}{
The line style of the ERP line to be plotted.
}

  \item{cex.xaxis}{
Magnification factor of the x-axis labels.
}
  \item{cex.yaxis}{
Magnification factor of the y-axis labels.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plots a scalp map of of the ERP waveforms in an array resembling the topographic placements of electrodes on the scalp.
The waveforms represent the average of the first set of data frames specified. Timepoints in which thre is a positive (and significant) correlation are highlighted by red bars. Timepoints in which thre is a positive (and significant) correlation are highlighted by blue bars. The function returns also an object with the results of the correlatiosn saved. This object can be used in future plots with \code{erp.cor} or \code{scalp.cor}, by specifying it as the argument \code{sig}.
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
	erp.cor("Exp1_word_subj", 1:20, RT, Electrode="Fp1", ylim=10, legend=T)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
