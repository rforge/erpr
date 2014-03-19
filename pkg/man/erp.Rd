\name{erp}
\alias{erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
erp
}
\description{
A basic function to plot an ERP sweep of a single electrode.
}
\usage{
erp(e1, main = NULL, smo = 0.0, col = "black", startmsec = -200, endmsec = 1000, 
interval = c(startmsec, endmsec), step = 200, verticals = NULL, horizontals = NULL,
 x.axis = "default", ylim = "default", lwd = 1, lty = 1, cex.main = 1, cex.xaxis = 0.8,
  cex.yaxis = 1, y.axis = "default", frame.plot = TRUE, xaxis0 = FALSE, 
  xtick.l = "default", xticks = "default", yaxis0 = FALSE, 
  yaxis0.origin = 0, ytick.l = "default", y.axis.step = 2, 
  yticks = "default", yrev = T, tick = "low", lwd.xticks = 1, lwd.yticks = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
A vector with the electrode to be plotted
}
  \item{main}{
The title of the plot
}
  \item{smo}{
The smoothing parameter to be applied before plotting. The default is 0.
}
  \item{col}{
Color of the electrode plotted.
}
  \item{startmsec}{
A value, in milliseconds indicating the beginning time of the ERP vector
}
  \item{endmsec}{
A value, in milliseconds indicating the end time of the ERP vector
}
  \item{interval}{
A vector with a pair of values, indicating the interval to be plotted
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
  \item{cex.main}{
Magnification factor of the Title supplied in \code{Main}.
}
  \item{cex.xaxis}{
Magnification factor of the x-axis labels.
}
  \item{cex.yaxis}{
Magnification factor of the y-axis labels.
}
  \item{y.axis}{
If \code{default}, than an x.axis will be drawn on the bases of \code{y.lim} and \code{y.axis.step} parameters. A vector of values can be supplied instead.
}
  \item{frame.plot}{
logical. Default is \code{TRUE}. Draw a box around the plot.
}
  \item{xaxis0}{
logical. Default is \code{FALSE}. If \code{TRUE} all default R parameters on axis drawing are overridden. Another axis is drawn, with the parameters specified all parameters listed below.
}
  \item{xtick.l}{
The length of ticks in the x-axis.
}
  \item{xticks}{
Position of xticks, in milliseconds. If "default" they are automatically placed.
}
  \item{yaxis0}{
Logical. Default is \code{FALSE}. If \code{TRUE}. The y-axis default R parameters are ignored and another axis is drawn with the parameters specified below.
}
  \item{yaxis0.origin}{
Position (in ms) on which the y-axis intersect the x-axis.
}
  \item{ytick.l}{
The length of ticks in the x-axis.
}
  \item{y.axis.step}{
The distance between each tick in the y axis.
}
  \item{yticks}{
Position (in microVolt) where y ticks are drawn.
}
  \item{yrev}{
Logical. If \code{TRUE} the negative voltages is plotted upward.
}
  \item{tick}{
currently unused parameter
}
  \item{lwd.xticks}{
the line width of ticks on the x-axis.
}
  \item{lwd.yticks}{
the line width of ticks on the y-axis.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
#data(ERPsets)
# erp(Exp1_word_subj1$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-20,20))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
