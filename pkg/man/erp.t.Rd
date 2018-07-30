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
erp.t(base1, base2, numbers1, numbers2 = numbers1, startmsec = NULL, 
endmsec=NULL, electrode, smo = NULL, paired = TRUE, alpha = 0.05, 
erplist1 = NULL, erplist2 = erplist1, sig = NULL, main = electrode, 
col = c("blue", "red"), p.adjust.method="none", ...)
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
  \item{startmsec}{
the start time (in ms) of the ERP vector.
}
  \item{endmsec}{
the end time (in ms) of the ERP vector.
}

  \item{electrode}{
a string indicating the electrode to be considered in the analysis.
}

  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}

  \item{paired}{
logical. This argument specifies if the t-tests are paired or not.
}
  \item{alpha}{
the alpha of the t-tests.
}
  \item{erplist1}{
a list containing the ERP data frames specified in \code{base1} and \code{numbers1}.
}
  \item{erplist2}{
a list containing the ERP data frames specified in \code{base2} and \code{numbers2}.
}
  \item{sig}{
if \code{NULL}, then a series of t-tests is computed. If an appropriate object is specified (i.e. the object returned by \code{erp.t} or \code{scalp.t})
then the results are retrieved by this object and the function simply plots the results.
}
  \item{main}{
plot title.
}

 \item{col}{
the colors of the waveforms.
}

	  \item{p.adjust.method}{
p-value correction method as in \code{\link{p.adjust}}. Alternatively \code{"tmax"} can be used to perform permutation test as in Groppe et al., 2011. See \code{mass.t.test} for details.
}

\item{\ldots}{
	further parameters to be passed to \code{\link{erp}}.
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
data(erplistExample)

erp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, 
electrode="Fp1", ylim=c(-10,10), erplist1=erplistExample, 
erplist2=erplistExample, col=c("blue", "red"))
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
