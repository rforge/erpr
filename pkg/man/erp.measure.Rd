\name{erp.measure}
\alias{erp.measure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
erp.measure
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
erp.measure(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, fun=average.mean, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", ...)
	
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
function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, fun=average.mean, others=NULL, format="long", name.dep="Dep", name.newvar="Electrode", ...)
	{
	FUN=match.fun(fun)
	temp=FUN(base=base, numbers=numbers, win.ini=win.ini, win.end=win.end, env=env, startmsec=startmsec, endmsec=endmsec,...)
	
	if(format=="long"){
		
		temp=rearrange(deps=1:(length(temp)-2),oth=c("Subject", "Subject_name"),dataset=temp, name.dep=name.dep, name.newvar=name.newvar)
		
	}
	
	if (!is.null(others))
		{
			for (i in 1:length(others))
			{
			temp[, names(others)[i]]=others[i]
			}
		}
	return(temp)
	}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
