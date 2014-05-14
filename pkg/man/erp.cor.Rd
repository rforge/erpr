\name{erp.cor}
\alias{erp.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the timepoint correlations between ERPs (single electrode) and an external variable 
}
\description{
This function performs a series of correlations on each timepoint between ERP voltages and an external (typically a behavioral) variable. The set of ERP data frames is specified by \code{base} and \code{numbers}. The behavioral variable is specified as a vector (of length equal to \code{numbers}) as the argument \code{external}. The function returns a plot of a single electrode showing the results of the correlation tests.
}
\usage{
	erp.cor(base, numbers, electrode, erplist=NULL,startmsec=-200, 
	endmsec=1200, external=NULL, smo=NULL, alpha=0.05, 
	method = c("pearson", "kendall", "spearman"),  sig=NULL,  
	main=electrode, ...)
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

 \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}

  \item{startmsec}{
the start time (in ms) of the ERP vector.
}
  \item{endmsec}{
the end time (in ms) of the ERP vector.
}

  \item{external}{
a vector indicating the values of the external variable (typically a behavioural variable). This vector must be of the same length of \code{numbers}
}
  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
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
  \item{main}{
plot title.
}
\item{\ldots}{
further parameters to be passed to \code{link{erp}}.
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

#generate some simulated RT data 

RT=rnorm(20, mean = 500, sd = 100)
erp.cor("Exp1_word_subj", numbers=1:20, electrode="Fp1",  
erplist=ERPsets, external=RT, startmsec=-200, endmsec=1500, 
ylim=c(-6,6))
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
