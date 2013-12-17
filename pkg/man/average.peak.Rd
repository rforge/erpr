\name{average.peak}
\alias{average.peak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average.peak
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
average.peak(base, numbers, win.ini, wi.end, env = .GlobalEnv, startmsec = -200, endmsec = 1200, peaktype = "max")
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
  \item{wi.end}{
%%     ~~Describe \code{wi.end} here~~
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
function (base, numbers, win.ini, wi.end, env = .GlobalEnv, startmsec = -200, 
    endmsec = 1200, peaktype = "max") 
{
    datall = NULL
    msectopoints = function(a, lengthsegment, startmsec, endmsec) {
        x = ((a - (startmsec)) * (endmsec - 1))/(endmsec - (startmsec))
        return(x + 1)
    }
    for (i in 1:length(numbers)) {
        average.temp = eval(parse(file = "", text = paste(base, 
            numbers[i], sep = "")), env = env)
        Subject_name = comment(eval(parse(file = "", text = paste(base, 
            numbers[i], sep = "")), env = env))
        average.temp = apply(average.temp[round(msectopoints(win.ini, 
            dim(average.temp)[1], startmsec, endmsec)):round(msectopoints(wi.end, 
            dim(average.temp)[1], startmsec, endmsec)), ], 2, 
            eval(parse(file = "", text = peaktype)))
        average.temp = data.frame(t(average.temp))
        average.temp$Subject = numbers[i]
        average.temp$Subject_name = Subject_name
        datall = rbind(datall, average.temp)
    }
    rownames(datall) = 1:dim(datall)[1]
    return(datall)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
