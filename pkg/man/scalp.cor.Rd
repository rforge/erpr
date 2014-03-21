\name{scalp.cor}
\alias{scalp.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the timepoint correlations between ERPs and an external variable (scalp array)
}
\description{
This function performs a series of correlations on each timepoint between ERP voltages and another (typically a behavioural) variable. The set of ERP data frames is specified by \code{base} and \code{numbers}. The behavioral variable is specified as a vector (of length equal to \code{numbers}) in the argument \code{behaviour}. A scalp plot of the results is returned.
}
\usage{
scalp.cor(base, numbers, behaviour = NULL, alpha = 0.05, 
method = c("pearson", "kendall", "spearman"), sig = NULL, 
envir = .GlobalEnv, smo = 0.5, layout = 1, ylims = "auto",
 yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = c(1, 1), 
 lty = c(1, 1), col = "blue", legend = TRUE, legend.lab = "default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base}{
a string indicating the beginning of the name of the set of ERP data frames.
}
  \item{numbers}{
the numbers of the subjects of the set of ERP data frames .
}
  \item{behaviour}{
a vector indicating the values of the behavioural variable. This vector must be of the same length of \code{numbers}
}
  \item{alpha}{
the alpha of the correlation tests.
}
  \item{method}{
the method of the correlations. Available methods are \code{"pearson", "kendall", "spearman"}.
}
  \item{sig}{
if \code{NULL}, then a series of correlations is computed. If an appropriate object is specified (i.e. an object returned by \code{scalp.cor} or \code{erp.cor}) then the results are retrieved by this object and the function simply plots the results.
}
  \item{envir}{
the environment in which the ERP data frames are searched for.
}
 \item{smo}{
%%     ~~Describe \code{smo} here~~
}
  \item{layout}{
the layout of electrodes to be plotted. Currently only few layouts are supported. 
}
  \item{ylims}{
a number indicating the upper and lower limits of the y-axis. If "auto", the function calculates the y limits automatically.
}
  \item{yrev}{
logical. If \code{TRUE}, the y-axis is plotted with the negative values upward.
}
 \item{startmsec}{
the start time (in ms) of the ERP vector
}
  \item{endmsec}{
the end time (in ms) of the ERP vector
}

  \item{lwd}{
A vector specifying the line width to be associated with each ERP data frame specfied in \code{categ}
}
  \item{lty}{
A vector specifying the line types to be associated with each ERP data frame specfied in \code{categ}
}

\item{col}{
	the color of the ERP waveform.
}

  \item{legend}{
logical. If \code{TRUE}, a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
the legend labels. If "default", the names are retrieved from the \code{categ} argument.
}}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plots a scalp map of ERP waveforms in an array resembling the topographic locations of the electrodes on the scalp.
The waveforms represent an average of the first set of specified data frames. Timepoints with a positive (and significant) correlation are highlighted by red bars. Timepoints with a negative (and significant) correlation are highlighted by blue bars. The function also returns an object with the results of the correlations. This object can be used for future plots with \code{erp.cor} or \code{scalp.cor}, by specifying it as the argument \code{sig}.

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
\code{\link{erp.cor}}, ~~~
}
\examples{

\dontrun{
data(ERPsets)

#generate an hypothetic behavioral variables
RT=rnorm(20, mean=500, sd=200)

scalp.cor("Exp1_word_subj", 1:20, behaviour = RT, smo=0, layout=1, ylim=10, legend=T)

}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
