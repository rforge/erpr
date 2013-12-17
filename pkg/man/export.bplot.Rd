\name{export.bplot}
\alias{export.bplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
export.bplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
export.bplot(dat, Npts = dim(dat)[1], startmsec = -200, endmsec = 1200, SB = 1, SC = 500, outname = "Bplotfile.avr")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{Npts}{
%%     ~~Describe \code{Npts} here~~
}
  \item{startmsec}{
%%     ~~Describe \code{startmsec} here~~
}
  \item{endmsec}{
%%     ~~Describe \code{endmsec} here~~
}
  \item{SB}{
%%     ~~Describe \code{SB} here~~
}
  \item{SC}{
%%     ~~Describe \code{SC} here~~
}
  \item{outname}{
%%     ~~Describe \code{outname} here~~
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
function (dat, Npts = dim(dat)[1], startmsec = -200, endmsec = 1200, 
    SB = 1, SC = 500, outname = "Bplotfile.avr") 
{
    TSB = startmsec
    DI = (endmsec + abs(startmsec))/Npts
    dat = t(dat)
    write(paste("Npts=", Npts, "TSB=", TSB, "DI=", DI, "SB=", 
        SB, "SC=", SC, sep = " "), file = outname)
    write.table(dat, row.names = F, col.names = F, file = outname, 
        append = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
