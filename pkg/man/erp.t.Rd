\name{erp.t}
\alias{erp.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot t-test comparisons at all timepoints between two ERP set of data frames (single electrode)
}
\description{
This function performs a series of \code{t.test} on each timepoint comparing two sets of ERP data frames. The first set is specified by \code{base1} and \code{numbers}. The second is specified by \code{base2} and \code{numbers2}.
}
\usage{
erp.t(base1, base2, numbers, numbers2 = numbers, Electrode, paired = TRUE, 
alpha = 0.05, envir = .GlobalEnv, envir2 = NULL, sig = NULL, main = Electrode, 
smo = 0.5, col = "black", startmsec = -200, endmsec = 1200, interval = c(startmsec, endmsec), 
step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", ylim = c(-6, 6), 
lwd = c(1, 1), lty = c(1, 1), cex.xaxis = 0.8, cex.yaxis = 1, color.list = c("blue", "red"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base1}{
A string indicating the beginning of the name of the first set of ERP data frames.
}
\item{base2}{
A string indicating the beginning of the name of the second set of ERP data frames.
}
  \item{numbers}{
The numbers (indicating different subjects), of the first set of ERP data frames .
}
  \item{numbers2}{
The numbers (indicating different subjects), of the second set of ERP data frames .
}
  \item{Electrode}{
A string indicating the electrode to be considered in the analysis.
}
  \item{paired}{
Logical. This argument specifies if the t-tests are paired or not.
}
  \item{alpha}{
The alpha of the t-tests.
}
  \item{envir}{
The environment in which the ERP data frames of the first set are searched.
}
  \item{envir2}{
The environment in which the ERP data frames of the second set are searched.
}
  \item{sig}{
If \code{NULL}, then the series of t-test is computed. If an appropriate object is specified (i.e. the object returned by \code{erp.t} or \code{scalp.t})
then the results are retrieved by this object and the function simply plots the results.
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
  \item{color.list}{
a vector specifying the two colors of to be associated with the two conditions.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plot two ERP waveforms. One is the average of the first set of data frames specified, and the other is the average of the second set of data frames specified. The colors of the waveforms are specified in the argument \code{color.list}. Timepoints in which the t-test performed is significant are highlighted by gray bars. The function returns also an object with the results of the t-tests saved. This object can be used in future plots with \code{erp.t} or \code{scalp.t}, by specifying it as the argument \code{sig}.
}

\author{
Giorgio Arcara
}
\note{
Currently this function perform a series of t-tests for all the Electrodes, and not only for the requested electrode.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{scalp.t}}, ~~~

}
\examples{

\dontrun{
data(ERPsets)
erp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, smo=0, layout=1, ylim=c(-10,10), 
startmsec=-200, endmsec=1500, color.list=c("blue", "red"))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
