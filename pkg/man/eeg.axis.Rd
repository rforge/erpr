\name{eeg.axis}
\alias{eeg.axis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
eeg.axis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eeg.axis(e1, smo = 0.5, col = "black", startmsec = -200, endmsec = 1000, yrev = TRUE, step = 200, x.axis = "default", main = NULL, ylim = "default", lwd = 1, lty = 1, cex.xaxis = 0.8, cex.yaxis = 1, xaxis0 = TRUE, xtick.l = "default", yaxis0 = TRUE, yaxis0.origin = 0, ytick.l = "default", y.axis.step = 2, frame.plot = FALSE, xticks = "default", yticks = "default", x.dist = 2, y.dist = 2, xlim = c(startmsec, endmsec), ynumbers = "default", xnumbers = "default", lwd.xticks = 1, lwd.yticks = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
%%     ~~Describe \code{e1} here~~
}
  \item{smo}{
%%     ~~Describe \code{smo} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{startmsec}{
%%     ~~Describe \code{startmsec} here~~
}
  \item{endmsec}{
%%     ~~Describe \code{endmsec} here~~
}
  \item{yrev}{
%%     ~~Describe \code{yrev} here~~
}
  \item{step}{
%%     ~~Describe \code{step} here~~
}
  \item{x.axis}{
%%     ~~Describe \code{x.axis} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{cex.xaxis}{
%%     ~~Describe \code{cex.xaxis} here~~
}
  \item{cex.yaxis}{
%%     ~~Describe \code{cex.yaxis} here~~
}
  \item{xaxis0}{
%%     ~~Describe \code{xaxis0} here~~
}
  \item{xtick.l}{
%%     ~~Describe \code{xtick.l} here~~
}
  \item{yaxis0}{
%%     ~~Describe \code{yaxis0} here~~
}
  \item{yaxis0.origin}{
%%     ~~Describe \code{yaxis0.origin} here~~
}
  \item{ytick.l}{
%%     ~~Describe \code{ytick.l} here~~
}
  \item{y.axis.step}{
%%     ~~Describe \code{y.axis.step} here~~
}
  \item{frame.plot}{
%%     ~~Describe \code{frame.plot} here~~
}
  \item{xticks}{
%%     ~~Describe \code{xticks} here~~
}
  \item{yticks}{
%%     ~~Describe \code{yticks} here~~
}
  \item{x.dist}{
%%     ~~Describe \code{x.dist} here~~
}
  \item{y.dist}{
%%     ~~Describe \code{y.dist} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ynumbers}{
%%     ~~Describe \code{ynumbers} here~~
}
  \item{xnumbers}{
%%     ~~Describe \code{xnumbers} here~~
}
  \item{lwd.xticks}{
%%     ~~Describe \code{lwd.xticks} here~~
}
  \item{lwd.yticks}{
%%     ~~Describe \code{lwd.yticks} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (e1, smo = 0.5, col = "black", startmsec = -200, endmsec = 1000, 
    yrev = TRUE, step = 200, x.axis = "default", main = NULL, 
    ylim = "default", lwd = 1, lty = 1, cex.xaxis = 0.8, cex.yaxis = 1, 
    xaxis0 = TRUE, xtick.l = "default", yaxis0 = TRUE, yaxis0.origin = 0, 
    ytick.l = "default", y.axis.step = 2, frame.plot = FALSE, 
    xticks = "default", yticks = "default", x.dist = 2, y.dist = 2, 
    xlim = c(startmsec, endmsec), ynumbers = "default", xnumbers = "default", 
    lwd.xticks = 1, lwd.yticks = 1) 
{
    lengthwhole = length(e1)
    msectopoints = function(a, lengthsegment, baseline, total.length) {
        x = ((a - (baseline)) * (lengthsegment - 1))/(total.length - 
            (baseline))
        return(x + 1)
    }
    startpoint = msectopoints(xlim[1], lengthwhole, startmsec, 
        endmsec)
    endpoint = msectopoints(xlim[2], lengthwhole, startmsec, 
        endmsec)
    vet = seq(xlim[1], xlim[2], step)
    if (x.axis[1] != "default") {
        vet = x.axis
    }
    temp0 = msectopoints(0, lengthwhole, startmsec, endmsec)
    vet2 = msectopoints(vet, lengthwhole, startmsec, endmsec)
    vet.names = paste(vet)
    maxe1 = max(e1)
    mine1 = min(e1)
    if (ylim[1] == "default") {
        ylim = sort(range(c(-6, 6, maxe1, mine1)))
    }
    else {
        ylim = ylim
    }
    if (yticks[1] == "default") {
        yvet = seq(ylim[1], ylim[2], y.axis.step)
    }
    else {
        yvet = yticks
    }
    plot(smooth.spline(e1, spar = smo), type = "n", ylim = sort(ylim), 
        col = col, lwd = lwd, main = main, yaxt = "n", xaxt = "n", 
        xlim = c(startpoint, endpoint), ylab = "", xlab = "", 
        lty = lty, cex.axis = cex.yaxis, frame.plot = frame.plot, 
        )
    if (xtick.l == "default") {
        xtick.l = sum(abs(ylim)/40)
    }
    if (xaxis0 == FALSE) {
        axis(1, vet2, paste(vet), cex.axis = cex.xaxis)
        abline(h = 0, lty = "longdash")
        segments(temp0, -0.5, temp0, 0.5, lty = 1.5, lwd = lwd.xticks)
    }
    if (xticks[1] == "default") {
        xticks = vet
    }
    if (xnumbers[1] == "default") {
        xnumbers = xticks
    }
    if (xaxis0 == TRUE) {
        segments(msectopoints(xticks, length(e1), startmsec, 
            endmsec), -xtick.l, msectopoints(xticks, length(e1), 
            startmsec, endmsec), +xtick.l, lty = 1.5, lwd = lwd.xticks)
        abline(h = 0)
        text(msectopoints(xnumbers, length(e1), startmsec, endmsec), 
            rep(0 - (xtick.l * x.dist), length(xnumbers)), labels = xnumbers, 
            cex = cex.xaxis)
    }
    if (yaxis0 == FALSE) {
        axis(2, yvet, yvet, cex.axis = cex.xaxis)
    }
    if (yaxis0 == FALSE) {
        axis(2, seq(ylim[1], ylim[2], y.axis.step), seq(ylim[1], 
            ylim[2], y.axis.step), cex.axis = cex.xaxis)
    }
    if (ytick.l == "default") {
        ytick.l = length(e1)/40
    }
    if (yticks[1] == "default") {
        yticks = seq(ylim[1], ylim[2], y.axis.step)
    }
    if (ynumbers[1] == "default") {
        ynumbers = yticks
    }
    if (yaxis0 == TRUE) {
        abline(v = msectopoints(yaxis0.origin, length(e1), startmsec, 
            endmsec), lty = 1.5)
        segments(msectopoints(yaxis0.origin, length(e1), startmsec, 
            endmsec) - ytick.l, yticks, msectopoints(yaxis0.origin, 
            length(e1), startmsec, endmsec) + ytick.l, yticks, 
            lwd = lwd.yticks)
        text(rep(msectopoints(yaxis0.origin, length(e1), startmsec, 
            endmsec) - (ytick.l * y.dist), length(yticks)), ynumbers, 
            labels = ynumbers, cex = cex.xaxis)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
