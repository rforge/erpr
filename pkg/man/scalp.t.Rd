\name{scalp.t}
\alias{scalp.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scalp.t
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scalp.t(base1, base2, numbers, numbers2 = NULL, paired = TRUE, alpha = 0.05, sig = NULL, env = .GlobalEnv, env2 = NULL, smo = 0.5, label = c("type1"), layout = 1, ylims = "auto", yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = c(1, 1), lty = c(1, 1), color.list = c("blue", "red"), legend = F, legend.lab = "default", t.axis = seq(-100, endmsec, 200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base1}{
%%     ~~Describe \code{base1} here~~
}
  \item{base2}{
%%     ~~Describe \code{base2} here~~
}
  \item{numbers}{
%%     ~~Describe \code{numbers} here~~
}
  \item{numbers2}{
%%     ~~Describe \code{numbers2} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{env2}{
%%     ~~Describe \code{env2} here~~
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
function (base1, base2, numbers, numbers2 = NULL, paired = TRUE, 
    alpha = 0.05, sig = NULL, env = .GlobalEnv, env2 = NULL, 
    smo = 0.5, label = c("type1"), layout = 1, ylims = "auto", 
    yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = c(1, 
        1), lty = c(1, 1), color.list = c("blue", "red"), legend = F, 
    legend.lab = "default", t.axis = seq(-100, endmsec, 200)) 
{
    if (length(legend.lab) == 1 & legend.lab[1] == "default") {
        legend.lab = c(base1, base2)
    }
    msectopoints = function(a, lengthsegment) {
        x = ((a - (startmsec)) * lengthsegment)/(endmsec + abs(startmsec))
        return(x)
    }
    if (is.null(numbers2)) {
        numbers2 = numbers
    }
    if (is.null(env2)) {
        env2 = env
    }
    if (is.null(sig)) {
        element = function(x, row.i) {
            return(x[row.i, ])
        }
        alldata1.list = list(NULL)
        alldata2.list = list(NULL)
        for (i1 in 1:length(numbers)) {
            alldata1.list[[i1]] = eval(parse(file = "", text = paste(base1, 
                numbers[i1], sep = "")), env = env)
        }
        for (i2 in 1:length(numbers2)) {
            alldata2.list[[i2]] = eval(parse(file = "", text = paste(base2, 
                numbers2[i2], sep = "")), env = env2)
        }
        alltemp = list(NULL)
        length(alltemp) = dim(alldata1.list[[1]])[1]
        alltemp.results = list(NULL)
        length(alltemp.results) = dim(alldata1.list[[1]])[1]
        n.points.time = floor(seq(1, dim(alldata1.list[[1]])[1], 
            dim(alldata1.list[[1]])[1]/10))
        time.elapsed = 0
        cat("t-test results computation\n")
        for (k in 1:dim(alldata1.list[[1]])[1]) {
            temp1 = lapply(alldata1.list, function(x) {
                element(x, k)
            })
            temp1.1 = matrix(unlist(temp1), ncol = length(alldata1.list[[1]]), 
                byrow = TRUE)
            temp2 = lapply(alldata2.list, function(x) {
                element(x, k)
            })
            temp2.1 = matrix(unlist(temp2), ncol = length(alldata1.list[[1]]), 
                byrow = TRUE)
            alltemp[[k]][[1]] = temp1.1
            alltemp[[k]][[2]] = temp2.1
            temp.results.vet = NULL
            for (j in 1:dim(alltemp[[k]][[1]])[2]) {
                temp.results.vet[j] = (t.test(alltemp[[k]][[1]][, 
                  j], alltemp[[k]][[2]][, j], corr = F, paired = paired)$p.value) < 
                  alpha
            }
            alltemp.results[[k]] = temp.results.vet
            if (k \%in\% n.points.time) {
                cat(rep(".", 10 - time.elapsed), "\n")
                time.elapsed = time.elapsed + 1
            }
        }
        cat("\n")
        alltemp.results = matrix(unlist(alltemp.results), byrow = TRUE, 
            ncol = dim(alldata1.list[[1]])[2])
        alltemp.results = as.data.frame(alltemp.results)
        names(alltemp.results) = names(alldata1.list[[1]])
    }
    if (!is.null(sig)) {
        alltemp.results = sig
    }
    alldata1 = grandaverage(base = base1, numbers, env = env)
    alldata2 = grandaverage(base = base2, numbers2, env = env2)
    categ = list(alldata1, alldata2)
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
            "P8", "blank", "O1", "OZ", "O2", "blank")
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
                spar = smo), type = "n", ylim = c(yedge[1] + 
                yedge[1]/3, yedge[2] + (yedge[2]/3)), col = color.list[1], 
                main = "", ylab = "", xlab = "", cex.main = 0.85, 
                xlim = c(1, dim(categ[[1]])[1]), xaxt = "n", 
                yaxt = "n", frame.plot = FALSE, lwd = lwd[1], 
                lty = lty[1])
            abline(v = grep(TRUE, alltemp.results[, electrodes[i]]), 
                col = "lightgray")
            lines(smooth.spline(categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]], 
                spar = smo), col = color.list[1], cex.main = 0.85, 
                lwd = lwd[1], lty = lty[1])
            totalendmsec = endmsec + abs(startmsec)
            zeropoint = (abs(startmsec) * dim(categ[[1]])[1])/totalendmsec
            segments(x0 = zeropoint, y0 = -0.8, x1 = zeropoint, 
                y1 = 0.5, lwd = 1.5)
            abline(h = 0, lty = "longdash")
            mtext(electrodes[i], side = 3, line = -2)
            lines(smooth.spline(categ[[2]][[electrodes[i]]], 
                spar = smo), col = color.list[2], lwd = lwd[2], 
                lty = lty[2])
        }
    }
    par(oldpar)
    invisible(alltemp.results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
