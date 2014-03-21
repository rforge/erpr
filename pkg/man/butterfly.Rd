\name{butterfly}
\alias{butterfly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ERP butterfly plot
}
\description{
A butterfly plot of several ERP data frames.
}
\usage{
butterfly(base, numbers, electrode, add = FALSE, smo = 0.5, col = "black", 
startmsec = -200, endmsec = 1000, interval = c(startmsec, endmsec), 
step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", 
ylim = "default", lwd = 1, lty = 1, outline = NULL, out.col = "black", envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{base}{
a string indicating the beginning of the name of the data frame containing ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
 \item{electrode}{
 the electrode to be plotted.
 }
 
  \item{add}{
add the current plot to a previous plot}
  \item{smo}{
the smoothing parameter to apply.
}
  \item{col}{
the color of the waveforms in the butterfly plot.
}
 \item{startmsec}{
the start time (in ms) of the ERP data frames. 
It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
the end time (in ms) of the ERP data frames.
}
 \item{interval}{
a vector with a pair of values, indicating the interval to be plotted.
}
   \item{step}{
the step of the x-axis labels. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
the x-coordinates of optional vertical bars.
}
  \item{horizontals}{
The y coordinates of optional horizontal bars.
}
 \item{x.axis}{
if \code{default}, then an x.axis will be drawn on the basis of \code{interval} and \code{step} parameters. 
A vector of values can be supplied instead.
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

  \item{outline}{
the number of the subject to be outlined (it will be plotted with lwd=2).
}
  \item{out.col}{
the color of the outlined ERP waveform.
}
  \item{envir}{
the environment in which \code{butterfly} is to be evaluated.
}
}
%\details{
%%  ~~ If necessary, more details than in the description above ~~
%}
\value{
A butterfly plot.
}

\author{
Giorgio Arcara
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.infl}}, \code{\link{scalp.infl}}
}
\examples{

\dontrun{
data(ERPsets)
butterfly(base="Exp1_nonword_subj", numbers=1:20, electrode="Fp1", smo=0, 
startmsec=-200, endmsec=1500, outline=1, ylim=c(-20,20), out.col="red")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
