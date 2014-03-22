\name{erp}
\alias{erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot an ERP waveform
}
\description{
A basic function to plot an ERP waveform of a single electrode.
}
\usage{
erp(e1, main = NULL, smo = 0.0, col = "black", startmsec = -200, endmsec = 1000, 
interval = c(startmsec, endmsec), step = 200, verticals = NULL, horizontals = NULL,
 x.axis = "default", ylim = "default", lwd = 1, lty = 1, cex.main = 1, cex.xaxis = 0.8,
  cex.yaxis = 1, y.axis = "default", frame.plot = TRUE, xaxis0 = FALSE, 
  xtick.l = "default", xticks = "default", yaxis0 = FALSE, 
  yaxis0.origin = 0, ytick.l = "default", y.axis.step = 2, 
  yticks = "default", lwd.xticks = 1, lwd.yticks = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
a vector with the electrode to be plotted.
}
  \item{main}{
plot title.
}
  \item{smo}{
the smoothing parameter to apply before plotting. Default is 0.
}
  \item{col}{
the color of the electrode plotted.
}
  \item{startmsec}{
the start time (in ms) of the ERP vector.
}
  \item{endmsec}{
the end time (in ms) of the ERP vector.
}
  \item{interval}{
a vector with a pair of values, indicating the interval to be plotted.
}
  \item{step}{
the step of the x-axis labels. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
the x coordinates of optional vertical bars.
}
  \item{horizontals}{
the y coordinates of optional horizontal bars.
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
  \item{cex.main}{
magnification factor of the Title supplied in \code{Main}.
}
  \item{cex.xaxis}{
magnification factor of the x-axis labels.
}
  \item{cex.yaxis}{
magnification factor of the y-axis labels.
}
  \item{y.axis}{
if \code{default}, then an x.axis will be drawn on the basis of \code{y.lim} and \code{y.axis.step} parameters. A vector of values can be supplied instead.
}
  \item{frame.plot}{
logical. Default is \code{TRUE}. Draw a box around the plot.
}
  \item{xaxis0}{
logical. Default is \code{FALSE}. If \code{TRUE}, then default x-axis R parameters are overridden. Another axis is drawn using the parameters specified below.
}
  \item{xtick.l}{
the length of the ticks on the x-axis.
}
  \item{xticks}{
the position of xticks (in ms). If "default", they are placed automatically.
}
  \item{yaxis0}{
logical. Default is \code{FALSE}. If \code{TRUE}, the default y-axis R parameters are overridden. Another axis is drawn using the parameters specified below.
}
  \item{yaxis0.origin}{
the position (in ms) where x-axis and y-axis intersect.
}
  \item{ytick.l}{
the length of the ticks on the x-axis.
}
  \item{y.axis.step}{
the distance between the ticks on the y-axis.
}
  \item{yticks}{
a vector of positions of the y-ticks (in microVolts).
}

  \item{lwd.xticks}{
x-ticks line width.
}
  \item{lwd.yticks}{
y-ticks line width.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
the plot of an ERP waveform. 
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp.t}}, \code{\link{erp.infl}}
}
\examples{

\dontrun{

data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20) 
erp(word$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-10,10))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
