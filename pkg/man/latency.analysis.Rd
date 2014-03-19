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
Rugg
}
\author{
Giorgio Arcara
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#####
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
