\name{scalp.cor}
\alias{scalp.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scalp.cor
}
\description{
This function performs a series of correlations on each timepoint betwen ERP voltages and another (usually a behavioural) variable. The set of ERP data frames is specified by \code{base1} and \code{numbers}. The behavioral variable is specified as a vector (of length equal to \code{numbers}) as the argument \code{behaviour}. A scalp plot of the results is returned.
}
\usage{
scalp.cor(base1, numbers, behaviour = NULL, alpha = 0.05, method = c("pearson", "kendall", "spearman"), sig = NULL, envir = .GlobalEnv, smo = 0.5, layout = 1, ylims = "auto", yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = c(1, 1), lty = c(1, 1), col = "blue", legend = TRUE, legend.lab = "default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base1}{
A string indicating the beginning of the name of the set of ERP data frames.
}
  \item{numbers}{
The numbers (indicating different subjects), of the set of ERP data frames .
}
  \item{behaviour}{
A vector indicating the values of the behavioural variables. This vector must be of the same length of \code{numbers}
}
  \item{alpha}{
The alpha of the correlation tests.
}
  \item{method}{
The method of the correlations. Available methods are \code{"pearson", "kendall", "spearman"}.
}
  \item{sig}{
If \code{NULL}, then the series of correlations is computed. If an appropriate object is specified (i.e. the object returned by \code{scalp.cor} or \code{erp.cor}) then the results are retrieved by this object and the function simply plots the results.
}
  \item{envir}{
The environment in which the ERP data frames  are searched.
}

 \item{smo}{
%%     ~~Describe \code{smo} here~~
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

\item{col}{
	the color of the ERP waveform.
}

  \item{legend}{
Logical. If \code{TRUE} a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
The legend labels. If "default" names are retrieved from the \code{categ} argument.
}}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function plots a scalp map of of the ERP waveforms in an array resembling the topographic placements of electrodes on the scalp.
The waveforms represent the average of the first set of data frames specified. Timepoints in which thre is a positive (and significant) correlation are highlighted by red bars. Timepoints in which thre is a positive (and significant) correlation are highlighted by blue bars. The function returns also an object with the results of the correlatiosn saved. This object can be used in future plots with \code{erp.cor} or \code{scalp.cor}, by specifying it as the argument \code{sig}.

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

data(ERPsets)

#generate an hypothetic behavioral variables
RT=rnorm(20, mean=500, sd=200)

scalp.cor("Exp1_word_subj", 1:20, behaviour = RT, smo=0, layout=1, ylim=10, legend=T)



}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
