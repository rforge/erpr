\name{scalp.t}
\alias{scalp.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot t-test comparisons at all timepoints between two ERP set of data frames (scalp array)
}
\description{
This function performs a series of t-tests on each timepoint comparing two sets of ERP data frames. The first set is specified by \code{base1} and \code{numbers1}. The second is specified by \code{base2} and \code{numbers2}. A scalp plot of the results is returned.
}
\usage{
scalp.t(base1, base2, numbers1, numbers2 = NULL, paired = TRUE,
alpha = 0.05, sig = NULL, erplist1 = NULL, erplist2 = erplist1, 
smo = NULL, layout = 1, ylims = "auto", yrev = TRUE,
startmsec = NULL, endmsec = NULL, lwd = c(1, 1), lty = c(1, 1), 
color.list = c("blue", "red"), legend = F, legend.lab = "default",
t.axis = NULL, scalp.array=NULL, p.adjust.method="none")
}
\arguments{	
	\item{base1}{
a string indicating the beginning of the name of the first set of ERP data frames.
}
	\item{base2}{
a string indicating the beginning of the name of the second set of ERP data frames.
}
  \item{numbers1}{
the numbers of the subjects of the first set of ERP data frames .
}
  \item{numbers2}{
the numbers of the subjects of the second set of ERP data frames .
}
  \item{paired}{
logical. This argument specifies if the t-tests are paired or not.
}
  \item{alpha}{
the alpha of the t-tests.
}
  \item{sig}{
if \code{NULL}, then a series of t-test is computed. If an appropriate object is specified (i.e. an object returned by \code{erp.t} or \code{scalp.t})
then the results are retrieved by this object and the function simply plots the results.
}
  \item{erplist1}{
a list containing the ERP data frames specified in \code{base1} and \code{numbers1}.
}
  \item{erplist2}{
a list containing the ERP data frames specified in \code{base2} and \code{numbers2}.
}

  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}

  \item{layout}{
A number indicating the layout of electrodes to be plotted. Currently only few default layouts are supported (a number from 1 to 5).
Alternatively, a character vector specifying the layout may be supplied (see \code{Details}). 
}

  \item{ylims}{
a number indicating the upper and lower limits of the y-axis. If "auto", the function calculates the y-limits automatically.
}
  \item{yrev}{
logical. If \code{TRUE}, the y-axis is plotted with negative values upward.
}

 \item{startmsec}{
the start time (in ms) of the ERP vector
}
  \item{endmsec}{
the end time (in ms) of the ERP vector
}

  \item{lwd}{
a vector specifying the line width to be associated with each ERP data frame specified in \code{categ}
}
  \item{lty}{
a vector specifying the line types to be associated with each ERP data frame specified in \code{categ}
}
  \item{color.list}{
a vector specifying the colors to be associated with each ERP data frame specified in \code{categ}
}
  \item{legend}{
logical. If \code{TRUE}, a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
the legend labels. If "default", the names are retrieved from \code{categ}.
}
  \item{t.axis}{
the position of the ticks of an additional axis indicating the time. This axis is plotted in the bottom-left corner of the plot array.
}
	\item{scalp.array}{
This argument is vector of two values indicating the number of panels of the plot, as number of rows and number of columns respectively. If a default layout is specified, the number of panels is automatically computed. See also \code{Details}.
	}
	  \item{p.adjust.method}{
p-value correction method as in \code{\link{p.adjust}}. Alternatively \code{"tmax"} can be used to perform permutation test as in Groppe et al., 2011. See \code{mass.t.test} for details.
}

}


\details{ The layout parameter can be a number (from 1 to 5), that specifies some default electrode arrangements (currently only a few default arrangements are supported). Otherwise, the argument may be a character vector indicating the names of the electrodes to be plotted. The plot is divided in a number of panels as specified in \code{scalp.array}. The names specified will be plotted from top to bottom and from left to right. If \code{"blank"} is specified an empty panel will be plotted. If \code{"xaxis"} is specified a x axis indicating time will be plotted. If \code{"yaxis"} is specified a y axis indicating voltage will be plotted. 
}

\value{
The function plots a map of two series of ERP waveforms in an array resembling the topographic locations of the electrodes on the scalp. One series is an average of the first set of specified data frames, and the other series is the average of the second set of specified data frames. The colors of the waveforms are specified in \code{color.list}. Timepoints in which the t-test is significant are highlighted by grey bars. The function also returns an object with the results of the t-tests. This object can be used for future plots with \code{erp.t} or \code{scalp.t}, by specifying it as the argument \code{sig}. Significance is calculated by a call to \code{mass.t.test} function.
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
data(erplistExample)

scalp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, smo=0, 
layout=1, ylims=10, startmsec=-200, endmsec=1500, 
color.list=c("blue", "red"), erplist1=erplistExample, erplist2=erplistExample)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
