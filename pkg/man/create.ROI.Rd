\name{create.ROI}
\alias{create.ROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.ROI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.roi(datall, Electrode="Electrode", groups=NULL, roi.levels=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deps}{
%%     ~~Describe \code{deps} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{substr.l}{
%%     ~~Describe \code{substr.l} here~~
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
function(datall, Electrode="Electrode", groups=NULL, roi.levels=NULL ){
	if (!is.list(groups)){
		stop("groups must be a list!")
	}
	if (length(groups)!=length(roi.levels)){
		stop("groups and roi.levels must have the same length!")
	}
	
	roi=rep(NA, dim(datall)[1])
	for (i in 1:length(groups)){
		roi[datall[,Electrode]%in%groups[[i]]]=roi.levels[i]
	}
	
	return(roi)
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
