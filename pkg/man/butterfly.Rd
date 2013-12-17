\name{butterfly}
\alias{butterfly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
butterfly
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
butterfly(base, numbers, electrode, add = FALSE, smo = 0.5, col = "black", startmsec = -200, endmsec = 1000, yrev = FALSE, interval = c(startmsec, endmsec), step = 200, verticals = NULL, horizontals = NULL, x.axis = "default", ylim = "default", lwd = 1, lty = 1, outline = NULL, out.col = "black", envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{numbers}{
%%     ~~Describe \code{numbers} here~~
}
  \item{electrode}{
%%     ~~Describe \code{electrode} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
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
  \item{outline}{
%%     ~~Describe \code{outline} here~~
}
  \item{out.col}{
%%     ~~Describe \code{out.col} here~~
}
  \item{envir}{
%%     ~~Describe \code{envir} here~~
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
function (base, numbers, electrode, add = FALSE, smo = 0.5, col = "black", 
    startmsec = -200, endmsec = 1000, yrev = FALSE, interval = c(startmsec, 
        endmsec), step = 200, verticals = NULL, horizontals = NULL, 
    x.axis = "default", ylim = "default", lwd = 1, lty = 1, outline = NULL, 
    out.col = "black", envir = .GlobalEnv) 
{
    env = deparse(substitute(envir))
    numbers = numbers[-(!(numbers \%in\% outline))]
    if (add == FALSE) {
        i = 1
        eeg(eval(parse(file = "", text = paste(env, "$", base, 
            numbers[i], "$", electrode, sep = ""))), smo = smo, 
            col = col, startmsec = startmsec, endmsec = endmsec, 
            yrev = yrev, interval = interval, step = step, verticals = verticals, 
            horizontals = horizontals, x.axis = x.axis, ylim = ylim, 
            lwd = lwd, lty = lty)
        for (i in 2:length(numbers)) {
            comp.add(eval(parse(file = "", text = paste(env, 
                "$", base, numbers[i], "$", electrode, sep = ""))), 
                col = col, lwd = lwd, lty = lty, smo = smo)
        }
    }
    if (add == TRUE) {
        for (i in 1:length(numbers)) {
            comp.add(eval(parse(file = "", text = paste(env, 
                "$", base, numbers[i], "$", electrode, sep = ""))), 
                col = col, lwd = lwd, lty = lty, smo = smo)
        }
    }
    if (!is.null(outline)) {
        for (k in 1:length(outline)) {
            comp.add(eval(parse(file = "", text = paste(env, 
                "$", base, outline[k], "$", electrode, sep = ""))), 
                col = out.col, lty = lty, smo = smo, lwd = lwd + 
                  2)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
