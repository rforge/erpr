\name{erp.t}
\alias{erp.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot t-test comparisons at all timepoints between two sets of ERP data frames (single electrode)
}
\description{
This function performs a series of t-tests on each timepoint comparing two sets of ERP data frames. The first set is specified by \code{base1} and \code{numbers1}. The second set is specified by \code{base2} and \code{numbers2}.
}
\usage{
erp.t(base1, base2, numbers1, numbers2 = numbers1, electrode, 
paired = TRUE, alpha = 0.05, envir = .GlobalEnv, envir2 = NULL,
sig = NULL, main = electrode, smo = 0.5, col = "black", 
startmsec = -200, endmsec = 1200, interval = c(startmsec, endmsec), 
step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", 
ylim = c(-6, 6), lwd = c(1, 1), lty = c(1, 1), cex.xaxis = 0.8, 
cex.yaxis = 1, color.list = c("blue", "red"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base1}{
a string indicating the beginning of the name of the first set of ERP data frames.
}
\item{base2}{
a string indicating the beginning of the name of the second set of ERP data frames.
}
  \item{numbers1}{
the numbers of the subjects of the first set of ERP data frames.
}
  \item{numbers2}{
The numbers of the subjects of the second set of ERP data frames.
}
  \item{electrode}{
a string indicating the electrode to be considered in the analysis.
}
  \item{paired}{
logical. This argument specifies if the t-tests are paired or not.
}
  \item{alpha}{
the alpha of the t-tests.
}
  \item{envir}{
the environment in which the ERP data frames of the first set are searched for.
}
  \item{envir2}{
the environment in which the ERP data frames of the second set are searched for.
}
  \item{sig}{
if \code{NULL}, then a series of t-tests is computed. If an appropriate object is specified (i.e. the object returned by \code{erp.t} or \code{scalp.t})
then the results are retrieved by this object and the function simply plots the results.
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
a vector with a pair of values indicating the interval to be plotted.
}
  \item{step}{
the step of the x-axis labels. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
the x-coordinates of optional vertical bars.
}
  \item{horizontals}{
the y-coordinates of optional horizontal bars.
}
  \item{x.axis}{
if \code{default}, then an x.axis will be drawn on the basis of \code{interval} and \code{step} parameters. A vector of values can be supplied instead.
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
  \item{color.list}{
a vector specifying the two colors of to be associated with the two conditions.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plots two ERP waveforms. One is an average of the first set of specified data frames, and the other is an average of the second set of specified data frames. The colors of the waveforms are specified in \code{color.list}. Timepoints on which the t-test is significant are highlighted by grey bars. The function also returns an object with the results of the t-tests. This object can be used for future plots with \code{erp.t} or \code{scalp.t}, by specifying it as the argument \code{sig}.
}

\author{
Giorgio Arcara
}
\note{
There is also a function \code{\link{scalp.infl}} able to perform the t-tests on all the electrodes, and not only on a single one.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{scalp.t}}, ~~~

}
\examples{

\dontrun{
data(ERPsets)

erp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, electrode="Fp1", smo=0, ylim=c(-10,10), 
startmsec=-200, endmsec=1500, color.list=c("blue", "red"))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
