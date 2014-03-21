\name{scalp.t}
\alias{scalp.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot t-test comparisons at all timepoints between two ERP set of data frames (scalp array)
}
\description{
This function performs a series of \code{t.test} on each timepoint comparing two sets of ERP data frames. The first set is specified by \code{base1} and \code{numbers}. The second is specified by \code{base2} and \code{numbers2}. A scalp plot of the results is returned.
}
\usage{
scalp.t(base1, base2, numbers, numbers2 = NULL, paired = TRUE,
alpha = 0.05, sig = NULL, envir = .GlobalEnv, envir2 = NULL, 
smo = 0.5, layout = 1, ylims = "auto", yrev = TRUE,
startmsec = -200, endmsec = 1200, lwd = c(1, 1), lty = c(1, 1), 
color.list = c("blue", "red"), legend = F, legend.lab = "default",
t.axis = seq(-100, endmsec, 200))
}
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
  \item{paired}{
Logical. This argument specifies if the t-tests are paired or not.
}
  \item{alpha}{
The alpha of the t-tests.
}
  \item{sig}{
If \code{NULL}, then the series of t-test is computed. If an appropriate object is specified (i.e. the object returned by \code{erp.t} or \code{scalp.t})
then the results are retrieved by this object and the function simply plots the results.
}
  \item{envir}{
The environment in which the ERP data frames of the first set are searched.
}

  \item{envir2}{
The environment in which the ERP data frames of the second set are searched.
}

  \item{smo}{
The smoothing parameter to be applied before plotting. The default is 0.
}

  \item{layout}{
The layout of electrodes to be plotted. Currently only few layout are supported. 
}
  \item{ylims}{
A number indicating the upper and lower limits of the y axis. If "auto", the function calculate the y limits automatically.
}
  \item{yrev}{
Logical. If \code{TRUE} the y axis is plotted with the negative values upward.
}

 \item{startmsec}{
A value, in milliseconds indicating the beginning time of the ERP vector
}
  \item{endmsec}{
A value, in milliseconds indicating the end time of the ERP vector
}

  \item{lwd}{
A vector specifying the line width to be associated to each ERP data frame specfied in \code{categ}
}
  \item{lty}{
A vector specifying the line types to be associated to each ERP data frame specfied in \code{categ}
}
  \item{color.list}{
A vector specifying the colors to be associated to each ERP data frame specfied in \code{categ}
}
  \item{legend}{
Logical. If \code{TRUE} a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
The legend labels. If "default" names are retrieved from the \code{categ} argument.
}
  \item{t.axis}{
The ticks and sequence of an axis plotted in the bottom-left corner of the array.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plots a map of two series of ERP waveforms in an array resembling the topographic placements of electrodes on the scalp.
One series is the average of the first set of data frames specified, and the other series is the average of the second set of data frames specified. The colors of the waveforms are specified in the argument \code{color.list}. Timepoints in which the t-test performed is significant are highlighted by gray bars. The function returns also an object with the results of the t-tests saved. This object can be used in future plots with \code{erp.t} or \code{scalp.t}, by specifying it as the argument \code{sig}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.t}}
}
\examples{
\dontrun{
data(ERPsets)
scalp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, smo=0, layout=1, ylim=c(-10,10), startmsec=-200, endmsec=1500, color.list=c("blue", "red"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
