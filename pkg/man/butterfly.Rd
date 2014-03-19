\name{butterfly}
\alias{butterfly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
butterfly
}
\description{
A butterfly plot of several ERP data frames.
}
\usage{
butterfly(base, numbers, electrode, add = FALSE, smo = 0.5, col = "black", 
startmsec = -200, endmsec = 1000, yrev = FALSE, interval = c(startmsec, endmsec), 
step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", ylim = "default", 
lwd = 1, lty = 1, outline = NULL, out.col = "black", env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{base}{
A string indicating the beginning of the name of \code{data.frame} containg the ERP data.
}
  \item{numbers}{
The numbers (indicating different subjects), to be averaged by the function.
}
 
  \item{add}{
add the current plot to a previous plot}
  \item{smo}{
A smoothing parameter to be applied
}
  \item{col}{
color of the waveforms in the butterfly plot.
}
 \item{startmsec}{
A value indicating the initial time (in ms) of the ERP \code{data.frame} objects. It can be a negative value, indicating the baseline period.
}
  \item{endmsec}{
A value indicating the final time (in ms) of the ERP \code{data.frame} objects.
}
  \item{yrev}{
Logical. Should y-axis be plotted with negative values upward?
}
 \item{interval}{
A vector with a pair of values, indicating the interval to be plotted.
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
  \item{outline}{
The number of the ERP data frame to be outlined (it will be plotted with lwd=2).
}
  \item{out.col}{
The color of the outlined ERP waveform.
}
  \item{env}{
The environment in which \code{butterfly} is to be evaluated.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a butterfly plot.
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

butterfly(base="Exp1_nonword_subj", numbers=1:20, electrode="OZ", smo=0, startmsec=-200, endmsec=1500, outline=1, ylim=c(-50,50))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
