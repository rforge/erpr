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
a vector with the electrode to be plotted
}
  \item{main}{
the title of the plot
}
  \item{smo}{
the smoothing parameter to be applied before plotting. The default is 0.
}
  \item{col}{
the color of the electrode plotted.
}
  \item{startmsec}{
a value (in ms) indicating the start time of the ERP vector
}
  \item{endmsec}{
A value (in ms) indicating the end time of the ERP vector
}
  \item{interval}{
a vector with a pair of values, indicating the interval to be plotted
}
  \item{step}{
the step on the x-axis labels to be plotted. The axis will start from the first value of \code{interval}
}
  \item{verticals}{
the x coordinates of optional vertical bars to be plotted
}
  \item{horizontals}{
the y coordinates of optional vertical bars to be plotted
}
  \item{x.axis}{
if \code{default}, then an x.axis will be drawn on the bases of \code{interval} and \code{step} parameters. A vector of values can be supplied instead.
}
  \item{ylim}{
a pair of values indicating the y-axis limit.
}
  \item{lwd}{
the width of the ERP line to be plotted.
}
  \item{lty}{
the style of the ERP line to be plotted.
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
logical. Default is \code{FALSE}. If \code{TRUE} then all default R parameters on axis drawing are overridden. Another axis is drawn, using the parameters specified below.
}
  \item{xtick.l}{
the length of ticks in the x-axis.
}
  \item{xticks}{
the position of xticks (in ms). If "default" they are automatically placed.
}
  \item{yaxis0}{
logical. Default is \code{FALSE}. If \code{TRUE}, the y-axis default R parameters are ignored and another axis is drawn with the parameters specified below.
}
  \item{yaxis0.origin}{
the position (in ms) where x-axis and y-axis intersect.
}
  \item{ytick.l}{
length of ticks on the x-axis.
}
  \item{y.axis.step}{
the distance between each tick on the y-axis.
}
  \item{yticks}{
the position (in microVolt) where y ticks are drawn.
}

  \item{lwd.xticks}{
the line width of the ticks on the x-axis.
}
  \item{lwd.yticks}{
the line width of ticks on the y-axis.
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
erp(word$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-20,20))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
