\name{eeg.t}
\alias{eeg.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
eeg.t
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eeg.t(base1, base2, numbers, numbers2 = numbers, Electrode, paired = TRUE, alpha = 0.05, env = .GlobalEnv, env2 = NULL, sig = NULL, main = Electrode, smo = 0.5, col = "black", startmsec = -200, endmsec = 1200, interval = c(startmsec, endmsec), step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", ylim = c(-6, 6), lwd = c(1, 1), lty = c(1, 1), cex.xaxis = 0.8, cex.yaxis = 1, color.list = c("blue", "red"))
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
  \item{Electrode}{
%%     ~~Describe \code{Electrode} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{env2}{
%%     ~~Describe \code{env2} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
  \item{interval}{
%%     ~~Describe \code{interval} here~~
}
  \item{step}{
%%     ~~Describe \code{step} here~~
}
  \item{verticals}{
%%     ~~Describe \code{verticals} here~~
}
  \item{horizontals}{
%%     ~~Describe \code{horizontals} here~~
}
  \item{x.axis}{
%%     ~~Describe \code{x.axis} here~~
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
  \item{color.list}{
%%     ~~Describe \code{color.list} here~~
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
function (base1, base2, numbers, numbers2 = numbers, Electrode, 
    paired = TRUE, alpha = 0.05, env = .GlobalEnv, env2 = NULL, 
    sig = NULL, main = Electrode, smo = 0.5, col = "black", startmsec = -200, 
    endmsec = 1200, interval = c(startmsec, endmsec), step = 200, 
    verticals = NULL, horizontals = NULL, x.axis = "default", 
    ylim = c(-6, 6), lwd = c(1, 1), lty = c(1, 1), cex.xaxis = 0.8, 
    cex.yaxis = 1, color.list = c("blue", "red")) 
{
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
    e1 = alldata1[, Electrode]
    e2 = alldata2[, Electrode]
    lengthwhole = length(e1)
    msectopoints = function(a, lengthsegment) {
        x = ((a - (startmsec)) * lengthsegment)/(endmsec + abs(startmsec))
        return(x)
    }
    startpoint = msectopoints(interval[1], lengthwhole)
    endpoint = msectopoints(interval[2], lengthwhole)
    vet = seq(interval[1], interval[2], step)
    if (x.axis[1] != "default") {
        vet = x.axis
    }
    temp0 = msectopoints(0, lengthwhole)
    vet2 = msectopoints(vet, lengthwhole)
    vet.names = paste(vet)
    maxe1 = max(e1)
    mine1 = min(e1)
    plot(e1, type = "n", ylim = ylim, col = col, lwd = lwd, main = main, 
        xaxt = "n", xlim = c(startpoint, endpoint), ylab = "", 
        xlab = "", lty = lty, cex.axis = cex.yaxis)
    abline(v = grep(TRUE, alltemp.results[, Electrode]), col = "lightgray", 
        lwd = 3)
    lines(smooth.spline(e1, spar = smo), col = color.list[1], 
        lwd = lwd[1], type = "l", lty = lty[1])
    lines(smooth.spline(e2, spar = smo), col = color.list[2], 
        lwd = lwd[2], type = "l", lty = lty[2])
    axis(1, vet2, paste(vet), cex.axis = cex.xaxis)
    abline(h = 0, lty = "longdash")
    segments(temp0, -0.5, temp0, 0.5, lty = 1)
    for (i in 1:length(verticals)) {
        x = msectopoints(verticals[i], lengthwhole)
        abline(v = x)
    }
    for (i in 1:length(horizontals)) {
        x = (horizontals[i])
        abline(h = x)
    }
    print(color.list)
    invisible(alltemp.results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
