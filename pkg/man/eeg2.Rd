\name{eeg2}
\alias{eeg2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
eeg2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eeg2(e, smo = 0, xlim = c(startmsec - 100, endmsec + 100), ylim = c(-8, 8), startmsec = -200, endmsec = 1200, interval = c(startmsec, endmsec), xaxis.pos = -6, xaxis.lim = c(startmsec, endmsec), xnumbers = seq(xaxis.lim[1], xaxis.lim[2], 200), xnumbers.dist = sum(abs(ylim)/20), xnumbers.pos = "down", cex.xnumbers = 1, xticks = seq(xaxis.lim[1], xaxis.lim[2], 200), xticks.l2 = "default", xticks.l1 = "default", xaxis.lwd = 1, xticks.lwd = 1, yaxis.pos = 0, yaxis.lim = ylim + c(2, -2), ynumbers = seq(min(yaxis.lim) + 2, max(yaxis.lim) - 2, 2), ynumbers.dist = "default", ynumbers.pos = "left", cex.ynumbers = 1, yticks = seq(min(yaxis.lim), max(yaxis.lim), 2), yticks.l1 = c("default"), yticks.l2 = c("default"), yaxis.lwd = 1, yticks.lwd = 1, details = F, frame.plot = F, v = NULL, h = NULL, v.lty = 1, h.lty = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{
%%     ~~Describe \code{e} here~~
}
  \item{smo}{
%%     ~~Describe \code{smo} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{startmsec}{
%%     ~~Describe \code{startmsec} here~~
}
  \item{endmsec}{
%%     ~~Describe \code{endmsec} here~~
}
  \item{interval}{
%%     ~~Describe \code{interval} here~~
}
  \item{xaxis.pos}{
%%     ~~Describe \code{xaxis.pos} here~~
}
  \item{xaxis.lim}{
%%     ~~Describe \code{xaxis.lim} here~~
}
  \item{xnumbers}{
%%     ~~Describe \code{xnumbers} here~~
}
  \item{xnumbers.dist}{
%%     ~~Describe \code{xnumbers.dist} here~~
}
  \item{xnumbers.pos}{
%%     ~~Describe \code{xnumbers.pos} here~~
}
  \item{cex.xnumbers}{
%%     ~~Describe \code{cex.xnumbers} here~~
}
  \item{xticks}{
%%     ~~Describe \code{xticks} here~~
}
  \item{xticks.l2}{
%%     ~~Describe \code{xticks.l2} here~~
}
  \item{xticks.l1}{
%%     ~~Describe \code{xticks.l1} here~~
}
  \item{xaxis.lwd}{
%%     ~~Describe \code{xaxis.lwd} here~~
}
  \item{xticks.lwd}{
%%     ~~Describe \code{xticks.lwd} here~~
}
  \item{yaxis.pos}{
%%     ~~Describe \code{yaxis.pos} here~~
}
  \item{yaxis.lim}{
%%     ~~Describe \code{yaxis.lim} here~~
}
  \item{ynumbers}{
%%     ~~Describe \code{ynumbers} here~~
}
  \item{ynumbers.dist}{
%%     ~~Describe \code{ynumbers.dist} here~~
}
  \item{ynumbers.pos}{
%%     ~~Describe \code{ynumbers.pos} here~~
}
  \item{cex.ynumbers}{
%%     ~~Describe \code{cex.ynumbers} here~~
}
  \item{yticks}{
%%     ~~Describe \code{yticks} here~~
}
  \item{yticks.l1}{
%%     ~~Describe \code{yticks.l1} here~~
}
  \item{yticks.l2}{
%%     ~~Describe \code{yticks.l2} here~~
}
  \item{yaxis.lwd}{
%%     ~~Describe \code{yaxis.lwd} here~~
}
  \item{yticks.lwd}{
%%     ~~Describe \code{yticks.lwd} here~~
}
  \item{details}{
%%     ~~Describe \code{details} here~~
}
  \item{frame.plot}{
%%     ~~Describe \code{frame.plot} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{v.lty}{
%%     ~~Describe \code{v.lty} here~~
}
  \item{h.lty}{
%%     ~~Describe \code{h.lty} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (e, smo = 0, xlim = c(startmsec - 100, endmsec + 100), 
    ylim = c(-8, 8), startmsec = -200, endmsec = 1200, interval = c(startmsec, 
        endmsec), xaxis.pos = -6, xaxis.lim = c(startmsec, endmsec), 
    xnumbers = seq(xaxis.lim[1], xaxis.lim[2], 200), xnumbers.dist = sum(abs(ylim)/20), 
    xnumbers.pos = "down", cex.xnumbers = 1, xticks = seq(xaxis.lim[1], 
        xaxis.lim[2], 200), xticks.l2 = "default", xticks.l1 = "default", 
    xaxis.lwd = 1, xticks.lwd = 1, yaxis.pos = 0, yaxis.lim = ylim + 
        c(2, -2), ynumbers = seq(min(yaxis.lim) + 2, max(yaxis.lim) - 
        2, 2), ynumbers.dist = "default", ynumbers.pos = "left", 
    cex.ynumbers = 1, yticks = seq(min(yaxis.lim), max(yaxis.lim), 
        2), yticks.l1 = c("default"), yticks.l2 = c("default"), 
    yaxis.lwd = 1, yticks.lwd = 1, details = F, frame.plot = F, 
    v = NULL, h = NULL, v.lty = 1, h.lty = 1, ...) 
{
    msectopoints = function(a, lengthsegment, startmsec, endmsec) {
        x = ((a - (startmsec)) * (lengthsegment - 1))/(endmsec - 
            (startmsec))
        return(x + 1)
    }
    startpoint = msectopoints(startmsec, length(e), startmsec, 
        endmsec)
    endpoint = msectopoints(endmsec, length(e), startmsec, endmsec)
    startpoint.l = msectopoints(interval[1], length(e), startmsec, 
        endmsec)
    endpoint.l = msectopoints(interval[2], length(e), startmsec, 
        endmsec)
    erp = smooth.spline(e[startpoint.l:endpoint.l], spar = smo)
    plot(startpoint.l:endpoint.l, erp$y, type = "l", yaxt = "n", 
        xaxt = "n", xlim = msectopoints(xlim, length(e), startmsec, 
            endmsec), ylab = "", xlab = "", ylim = ylim, frame.plot = frame.plot, 
        ...)
    segments(msectopoints(xaxis.lim[1], length(e), startmsec, 
        endmsec), xaxis.pos, msectopoints(xaxis.lim[2], length(e), 
        startmsec, endmsec), xaxis.pos, lwd = xaxis.lwd)
    xticks.l1[xticks.l1 \%in\% "default"] = -sum(abs(ylim)/40)
    xticks.l1 = as.numeric(xticks.l1)
    xticks.l2[xticks.l2 \%in\% "default"] = sum(abs(ylim)/40)
    xticks.l2 = as.numeric(xticks.l2)
    segments(msectopoints(xticks, length(e), startmsec, endmsec), 
        xticks.l1 + xaxis.pos, msectopoints(xticks, length(e), 
            startmsec, endmsec), xticks.l2 + xaxis.pos, lwd = xticks.lwd)
    if (xnumbers.pos == "up") {
        xadd = max(xticks.l2)
        xcoeff = 1
    }
    if (xnumbers.pos == "down") {
        xadd = min(xticks.l1)
        xcoeff = -1
    }
    text(msectopoints(xnumbers, length(e), startmsec, endmsec), 
        xaxis.pos + xadd + (xnumbers.dist * xcoeff), labels = xnumbers, 
        cex = cex.xnumbers)
    segments(msectopoints(yaxis.pos, length(e), startmsec, endmsec), 
        yaxis.lim[1], msectopoints(yaxis.pos, length(e), startmsec, 
            endmsec), yaxis.lim[2], lwd = yaxis.lwd)
    if (yticks.l2 == "default") {
        yticks.l2 = msectopoints(abs(xlim[1] + xlim[2]), length(e), 
            startmsec, endmsec)/80
    }
    yticks.l2 = as.numeric(yticks.l2)
    if (yticks.l1 == "default") {
        yticks.l1 = -msectopoints(abs(xlim[1] + xlim[2]), length(e), 
            startmsec, endmsec)/80
    }
    yticks.l1 = as.numeric(yticks.l1)
    segments(msectopoints(yaxis.pos, length(e), startmsec, endmsec) + 
        yticks.l1, yticks, yticks.l2 + msectopoints(yaxis.pos, 
        length(e), startmsec, endmsec), yticks, lwd = yticks.lwd)
    if (ynumbers.dist == "default") {
        ynumbers.dist = msectopoints(abs(xlim[1] + xlim[2]), 
            length(e), startmsec, endmsec)/20
    }
    if (ynumbers.pos == "right") {
        yadd = max(yticks.l2)
        ycoeff = 1
    }
    if (ynumbers.pos == "left") {
        yadd = min(yticks.l1)
        ycoeff = -1
    }
    text((msectopoints(yaxis.pos, length(e), startmsec, endmsec) + 
        yadd + (ynumbers.dist * ycoeff)), ynumbers, labels = ynumbers, 
        cex = cex.ynumbers)
    abline(h = h, lty = h.lty)
    abline(v = msectopoints(v, length(e), startmsec, endmsec), 
        lty = v.lty)
    if (details == TRUE) {
        cat("\n\ncurrent sampling rate:", 1000 * (length(e) - 
            1)/sum(-startmsec, endmsec), "Hz", "\n\n")
        cat("current xlims are: ", xlim[1], ",", xlim[2], "\n")
        cat("current ylims are: ", ylim[1], ",", ylim[2], "\n")
        cat("current xticks.l1 is: ", xticks.l1, "\n")
        cat("current xticks.l2 is: ", xticks.l2, "\n")
        cat("current xnumbers.dist is: ", xnumbers.dist, "\n")
        cat("current yticks.l1 is: ", yticks.l1, "\n")
        cat("current yticks.l2 is: ", yticks.l2, "\n")
        cat("current ynumbers.dist is: ", ynumbers.dist, "\n")
        cat("current x startpoint : ", startpoint.l, "\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
