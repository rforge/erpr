\name{latency.analysis}
\alias{latency.analysis}
\title{
latency.analysis
}
\description{
function to perform a latency analysis according to the procedure by Rugg et al. (1995).
}
\usage{
latency.analysis(base1, base2 = NULL, numbers1, numbers2 = numbers1, Electrodes, win.ini, win.end, paired = T, env = .GlobalEnv, baseline = -200, total.length = 1200, crit.msec = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base1}{
The beginning segment of the first group of object names of ERP data (with electrodes in separate columns).
}
  \item{base2}{
The beginning segment of the second group of object names of ERP data (with electrodes in separate columns). Default is \code{NULL}
}
  \item{numbers1}{
The numbers indicating the objects to be considered of group defined by \code{base1}. 
}
  \item{numbers2}{
The numbers indicating the objects to be considered of group defined by \code{base1}.
}
  \item{Electrodes}{
%%     ~~Describe \code{Electrodes} here~~
}
  \item{win.ini}{
%%     ~~Describe \code{win.ini} here~~
}
  \item{win.end}{
%%     ~~Describe \code{win.end} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{baseline}{
%%     ~~Describe \code{baseline} here~~
}
  \item{total.length}{
%%     ~~Describe \code{total.length} here~~
}
  \item{crit.msec}{
%%     ~~Describe \code{crit.msec} here~~
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
function (base1, base2 = NULL, numbers1, numbers2 = numbers1, 
    Electrodes, win.ini, win.end, paired = T, env = .GlobalEnv, 
    baseline = -200, total.length = 1200, crit.msec = 50) 
{
    dat.list = NULL
    msectopoint.values = function(a, lengthsegment, startmsec, 
        endmsec) {
        x = ((a - (startmsec)) * (lengthsegment - 1))/(endmsec - 
            (startmsec))
        return(x + 1)
    }
    temp1 = eval(parse(file = "", text = paste(base1, numbers1[1], 
        sep = "")), env = env)
    tot.timepoint.values = dim(temp1)[1]
    samp.rate = sampling.rate(temp1, baseline, total.length)
    crit.point.values = round(crit.msec/(1000/samp.rate))
    exact.crit.msec = crit.point.values * (1000/samp.rate)
    allsubj_cond1 = NULL
    for (i in 1:length(numbers1)) {
        temp1a = eval(parse(file = "", text = paste(base1, numbers1[i], 
            sep = "")), env = env)
        temp1b = apply(temp1a[msectopoint.values(win.ini, dim(temp1a)[1], 
            baseline, total.length):msectopoint.values(win.end, 
            dim(temp1a)[1], baseline, total.length), Electrodes], 
            1, mean)
        allsubj_cond1 = cbind(allsubj_cond1, temp1b)
    }
    if (!(is.null(base2))) {
        allsubj_cond2 = NULL
        for (i in 1:length(numbers2)) {
            temp2a = eval(parse(file = "", text = paste(base2, 
                numbers2[i], sep = "")), env = env)
            temp2b = apply(temp2a[msectopoint.values(win.ini, 
                dim(temp1a)[1], baseline, total.length):msectopoint.values(win.end, 
                dim(temp2a)[1], baseline, total.length), Electrodes], 
                1, mean)
            allsubj_cond2 = cbind(allsubj_cond2, temp2b)
        }
    }
    t.result.values = NULL
    if (paired == F | (paired == T & !(is.null(base2)))) {
        for (i in 1:dim(allsubj_cond1)[1]) {
            t.result.values[i] = t.test(allsubj_cond1[i, ], allsubj_cond2[i, 
                ], var.equal = T, paired = paired)$statistic
        }
        dfs = as.numeric(t.test(allsubj_cond1[i, ], allsubj_cond2[i, 
            ], paired = paired)$parameter)
        lat.pos = nhigher(t.result.values, n = crit.point.values, 
            criterion = qt(0.05, df = dfs, lower.tail = F))
        lat.neg = nhigher(-t.result.values, n = crit.point.values, 
            criterion = qt(0.05, df = dfs, lower.tail = F))
        lat.pos.msec = win.ini + (lat.pos - 1) * 1000/samp.rate
        lat.neg.msec = win.ini + (lat.neg - 1) * 1000/samp.rate
        return(list(t.values = t.result.values, t.crit = qt(0.05, 
            df = dfs), df = dfs, crit.msec = crit.msec, exact.crit.msec = exact.crit.msec, 
            exact.crit.point.values = crit.point.values, lat.pos.points = lat.pos, 
            lat.neg.points = lat.neg, LATENCY.pos = lat.pos.msec, 
            LATENCY.neg = lat.neg.msec))
    }
    if (paired == T & is.null(base2)) {
        for (i in 1:dim(allsubj_cond1)[1]) {
            t.result.values[i] = t.test(allsubj_cond1[i, ], var.equal = T)$statistic
        }
        dfs = t.test(allsubj_cond1[i, ])$parameter
        lat.pos = nhigher(t.result.values, n = crit.point.values, 
            criterion = qt(0.05, df = dfs, lower.tail = F))
        lat.neg = nhigher(-t.result.values, n = crit.point.values, 
            criterion = qt(0.05, df = dfs, lower.tail = F))
        lat.pos.msec = win.ini + (lat.pos - 1) * 1000/samp.rate
        lat.neg.msec = win.ini + (lat.neg - 1) * 1000/samp.rate
        return(list(t.values = t.result.values, t.crit = qt(0.05, 
            df = dfs, lower.tail = F), df = dfs, crit.msec = crit.msec, 
            exact.crit.msec = exact.crit.msec, exact.crit.point.values = crit.point.values, 
            lat.pos.points = lat.pos, lat.neg.points = lat.neg, 
            LATENCY.pos = lat.pos.msec, LATENCY.neg = lat.neg.msec))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
