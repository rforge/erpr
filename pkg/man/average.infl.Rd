\name{average.infl}
\alias{average.infl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average.infl
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
average.infl(base, numbers, electrode, smo = 0, layout = 1, ylims = 12, yrev = FALSE, startmsec = -200, endmsec = 1200, lwd = 1, lty = 1, env = .GlobalEnv)
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
  \item{smo}{
%%     ~~Describe \code{smo} here~~
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
  \item{env}{
%%     ~~Describe \code{env} here~~
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
function (base, numbers, electrode, smo = 0, layout = 1, ylims = 12, 
    yrev = FALSE, startmsec = -200, endmsec = 1200, lwd = 1, 
    lty = 1, env = .GlobalEnv) 
{
    require(rpanel)
    average.infl.panel = function(panel) {
        average.infl.endo = function(base, numbers, electrode, 
            outline, smo = 0, col = "black", startmsec = -200, 
            endmsec = 1000, yrev = FALSE, interval = c(startmsec, 
                endmsec), step = 200, verticals = NULL, horizontals = NULL, 
            x.axis = "default", ylim = c(-10, 10), lwd = 1, lty = 1, 
            out.col = "red", env) {
            average.temp = eval(parse(file = "", text = paste(base, 
                numbers[1], "$", electrode, sep = "")), envir = env)
            for (i in 2:length(numbers)) {
                average.temp = average.temp + eval(parse(file = "", 
                  text = paste(base, numbers[i], "$", electrode, 
                    sep = "")), envir = env)
            }
            average = average.temp/length(numbers)
            eeg(average, smo = smo, col = col, startmsec = startmsec, 
                endmsec = endmsec, yrev = yrev, interval = interval, 
                step = step, verticals = verticals, horizontals = horizontals, 
                x.axis = x.axis, ylim = ylim, lwd = lwd, lty = lty, 
                main = electrode)
            average.excl = (average.temp - eval(parse(file = "", 
                text = paste(base, outline, "$", electrode, sep = "")), 
                envir = env))/(length(numbers) - 1)
            comp.add(average.excl, col = out.col, lwd = 2, smo = smo)
            legend("topright", legend = c("Average all", paste("Average no subj", 
                outline)), pch = 15, col = c(col, out.col), pt.bg = c(1:6), 
                cex = 1.2)
        }
        average.infl.endo(base = base, numbers = numbers, electrode = electrode, 
            outline = panel$outnumber, smo = smo, yrev = yrev, 
            startmsec = startmsec, endmsec = endmsec, env = env)
        panel
    }
    panel <- rp.control()
    rp.listbox(panel, outnumber, numbers, labels = as.character(numbers), 
        action = average.infl.panel, initval = numbers[1])
    rp.do(panel, average.infl.panel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
