\name{scalp}
\alias{scalp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scalp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scalp(categ, smo = 0.5, label = c("type1"), layout = 1, ylims = "auto", yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = 1, lty = 1, color.list = c("blue", "red", "darkgreen"), legend = F, legend.lab = "default", t.axis = seq(-100, endmsec, 200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{
%%     ~~Describe \code{categ} here~~
}
  \item{smo}{
%%     ~~Describe \code{smo} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{layout}{
%%     ~~Describe \code{layout} here~~
}
  \item{ylims}{
%%     ~~Describe \code{ylims} here~~
}
  \item{yrev}{
%%     ~~Describe \code{yrev} here~~
}
  \item{startmsec}{
%%     ~~Describe \code{startmsec} here~~
}
  \item{endmsec}{
%%     ~~Describe \code{endmsec} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{color.list}{
%%     ~~Describe \code{color.list} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{legend.lab}{
%%     ~~Describe \code{legend.lab} here~~
}
  \item{t.axis}{
%%     ~~Describe \code{t.axis} here~~
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
function (categ, smo = 0.5, label = c("type1"), layout = 1, ylims = "auto", 
    yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = 1, lty = 1, 
    color.list = c("blue", "red", "darkgreen"), legend = F, legend.lab = "default", 
    t.axis = seq(-100, endmsec, 200)) 
{
    if (length(legend.lab) == 1 & legend.lab[1] == "default") {
        legend.lab = deparse(substitute(categ))
        legend.lab = gsub("\\(", "", legend.lab)
        legend.lab = gsub("\\)", "", legend.lab)
        legend.lab = gsub("^list", "", legend.lab)
        legend.lab = gsub(" ", "", legend.lab)
        legend.lab = unlist(strsplit(legend.lab, ","))
    }
    msectopoints = function(a, lengthsegment) {
        x = ((a - (startmsec)) * lengthsegment)/(endmsec + abs(startmsec))
        return(x)
    }
    if (length(lwd) == 1) {
        lwd = rep(lwd, length(categ))
    }
    if (length(lty) == 1) {
        lty = rep(lty, length(categ))
    }
    if (class(categ) != "list") {
        stop("input object must be a list!!")
    }
    if (layout == 1) {
        electrodes = c("axes", "Fp1", "blank", "Fp2", "legend", 
            "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", 
            "FC4", "FT8", "T3", "C3", "CZ", "C4", "T4", "TP7", 
            "CP3", "CPZ", "CP4", "TP8", "T5", "P3", "PZ", "P4", 
            "T6", "axis", "O1", "OZ", "O2", "blank")
    }
    if (layout == 2) {
        electrodes = c("axes", "Fp1", "FPZ", "Fp2", "legend", 
            "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", 
            "FC4", "FT8", "T7", "C3", "CZ", "C4", "T8", "TP7", 
            "CP3", "CPZ", "CP4", "TP8", "P7", "P3", "PZ", "P4", 
            "P8", "axis", "O1", "OZ", "O2", "blank")
    }
    if (layout == 3) {
        electrodes = c("axes", "Fp1", "Fpz", "Fp2", "legend", 
            "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCz", 
            "FC4", "FT8", "T3", "C3", "Cz", "C4", "T4", "TP7", 
            "CP3", "CPz", "CP4", "TP8", "T5", "P3", "PZ", "P4", 
            "T6", "axis", "O1", "blank", "O2", "blank")
    }
    if (layout == 4) {
        electrodes = c("axes", "Fp1", "blank", "Fp2", "legend", 
            "blank", "AF3", "blank", "AF4", "blank", "F7", "F3", 
            "Fz", "F4", "F8", "FC5", "FC1", "FCz", "FC2", "FC6", 
            "T7", "C3", "Cz", "C4", "T8", "blank", "CP1", "CPz", 
            "CP2", "blank", "P7", "P3", "Pz", "P4", "P8", "blank", 
            "O1", "blank", "O2", "blank")
    }
    if (layout == 5) {
        electrodes = c("axes", "Fp1", "Fpz", "Fp2", "legend", 
            "blank", "AF3", "blank", "AF4", "blank", "F7", "F3", 
            "Fz", "F4", "F8", "FC5", "FC1", "blank", "FC2", "FC6", 
            "T7", "C3", "Cz", "C4", "T8", "CP5", "CP1", "blank", 
            "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8", "PO7", 
            "PO3", "POz", "PO4", "PO8", "blank", "O1", "Oz", 
            "O2", "blank")
    }
    if (ylims == "auto") {
        alldata = NULL
        for (i in 1:length(categ)) {
            alldata = rbind(alldata, categ[[i]])
        }
        ymax = max(alldata)
        ymin = min(alldata)
        yedge = max(c(ymax, abs(ymin)))
        yedge = c(-yedge, yedge)
    }
    if (ylims != "auto") {
        yedge = ylims
        yedge = c(-ylims, ylims)
    }
    if (yrev == TRUE) {
        yedge = sort(yedge, decreasing = T)
    }
    oldpar <- par(no.readonly = TRUE)
    par(mfrow = c(7, 5), mai = c(0, 0, 0, 0))
    if (layout == 5) {
        par(mfrow = c(10, 5), mai = c(0, 0, 0, 0))
    }
    if (layout == 4) {
        par(mfrow = c(8, 5), mai = c(0, 0, 0, 0))
    }
    plot(categ[[1]]$P4, type = "n", frame.plot = FALSE, xlim = c(1, 
        dim(categ[[1]])[1]), xaxt = "n", yaxt = "n", ylim = c(yedge[1] + 
        yedge[1]/3, yedge[2] + (yedge[2]/3)))
    axis(side = 2, pos = dim(categ[[1]])[1]/2, at = c(round(ceiling(yedge[1]), 
        0), round(ceiling(yedge[1])/2, 0), 0, round(floor(yedge[2])/2, 
        0), round(floor(yedge[2]), 0)), cex.axis = 0.8, las = 2)
    text((dim(categ[[1]])[1]/2) + (dim(categ[[1]])[1]/8), 0, 
        labels = expression(paste(mu, "V")), cex = 1.4)
    for (i in 2:(length(electrodes))) {
        if (electrodes[i] == "blank") {
            plot.new()
        }
        if (electrodes[i] == "legend") {
            plot.new()
            if (legend == "TRUE") {
                legend("center", legend = legend.lab, col = color.list, 
                  cex = 1.2, lty = lty, lwd = lwd)
            }
        }
        if (electrodes[i] == "axis") {
            plot(categ[[1]]$P4, type = "n", frame.plot = FALSE, 
                xlim = c(1, dim(categ[[1]])[1]), xaxt = "n", 
                yaxt = "n", ylim = c(yedge[1] + yedge[1]/3, yedge[2] + 
                  (yedge[2]/3)))
            axis(1, pos = c(msectopoints(startmsec, dim(categ[[1]])[1]), 
                0), at = msectopoints(t.axis, dim(categ[[1]])[1]), 
                labels = paste(t.axis))
        }
        if (electrodes[i] != "blank" & electrodes[i] != "axis" & 
            electrodes[i] != "legend") {
            plot(smooth.spline(categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]], 
                spar = smo), type = "l", ylim = c(yedge[1] + 
                yedge[1]/3, yedge[2] + (yedge[2]/3)), col = color.list[1], 
                main = "", ylab = "", xlab = "", cex.main = 0.85, 
                xlim = c(1, dim(categ[[1]])[1]), xaxt = "n", 
                yaxt = "n", frame.plot = FALSE, lwd = lwd[1], 
                lty = lty[1])
            totalendmsec = endmsec + abs(startmsec)
            zeropoint = msectopoints(0, dim(categ[[1]])[1])
            segments(x0 = zeropoint, y0 = -0.8, x1 = zeropoint, 
                y1 = 0.5, lwd = 1.5)
            abline(h = 0, lty = "longdash")
            mtext(electrodes[i], side = 3, line = -2)
            if (length(categ) > 1 & electrodes[i] != "blank") {
                for (k in 2:length(categ)) {
                  lines(smooth.spline(categ[[k]][electrodes[i]], 
                    spar = smo), col = color.list[k], lwd = lwd[k], 
                    lty = lty[k])
                }
            }
        }
    }
    par(oldpar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
