\name{average.latency}
\alias{average.latency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average.latency
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
average.latency(base, numbers, win.ini, win.end, env = .GlobalEnv, startmsec = -200, endmsec = 1200, peaktype = "max")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{numbers}{
%%     ~~Describe \code{numbers} here~~
}
  \item{win.ini}{
%%     ~~Describe \code{win.ini} here~~
}
  \item{win.end}{
%%     ~~Describe \code{win.end} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{startmsec}{
%%     ~~Describe \code{startmsec} here~~
}
  \item{endmsec}{
%%     ~~Describe \code{endmsec} here~~
}
  \item{peaktype}{
%%     ~~Describe \code{peaktype} here~~
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
function (base, numbers, win.ini, win.end, env = .GlobalEnv, 
    startmsec = -200, endmsec = 1200, peaktype = "max") 
{
    datall = NULL
    msectopoints = function(a, lengthsegment, startmsec, endmsec) {
        x = ((a - (startmsec)) * (lengthsegment - 1))/(endmsec - 
            (startmsec))
        return(x + 1)
    }
    pointstomsec = function(a, lengthsegment, startmsec, endmsec) {
        x = (((endmsec + abs(startmsec)) * a)/(lengthsegment - 
            1)) - abs(startmsec)
        return(x + 1)
    }
    for (i in 1:length(numbers)) {
        average.temp = eval(parse(file = "", text = paste(base, 
            numbers[i], sep = "")), env = env)
        rownames(average.temp) = 1:dim(average.temp)[1]
        Subject_name = comment(eval(parse(file = "", text = paste(base, 
            numbers[i], sep = "")), env = env))
        average.temp.peak = apply(average.temp[round(msectopoints(win.ini, 
            dim(average.temp)[1], startmsec, endmsec)):round(msectopoints(win.end, 
            dim(average.temp)[1], startmsec, endmsec)), ], 2, 
            eval(parse(file = "", text = peaktype)))
        peak.pnts = NULL
        for (k in 1:length(average.temp)) {
            peak.pnts.temp = as.numeric(rownames(average.temp[average.temp[, 
                k] == average.temp.peak[k], ]))
            peak.pnts[k] = peak.pnts.temp[peak.pnts.temp >= round(msectopoints(win.ini, 
                dim(average.temp)[1], startmsec, endmsec)) & 
                peak.pnts.temp <= round(msectopoints(win.end, 
                  dim(average.temp)[1], startmsec, endmsec))][1]
        }
        peak.msec = pointstomsec(peak.pnts, dim(average.temp)[1], 
            startmsec, endmsec)
        average.temp.lat = data.frame(t(peak.msec))
        names(average.temp.lat) = names(average.temp)
        average.temp.lat$Subject = numbers[i]
        average.temp.lat$Subject_name = Subject_name
        datall = rbind(datall, average.temp.lat)
    }
    rownames(datall) = 1:dim(datall)[1]
    return(datall)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
