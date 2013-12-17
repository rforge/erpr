\name{load.eeg.multi}
\alias{load.eeg.multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
load.eeg.multi
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.eeg.multi(filenamebase, numbers, outname = "EEGdata_sogg", env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenamebase}{
%%     ~~Describe \code{filenamebase} here~~
}
  \item{numbers}{
%%     ~~Describe \code{numbers} here~~
}
  \item{outname}{
%%     ~~Describe \code{outname} here~~
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
function (filenamebase, numbers, outname = "EEGdata_sogg", env = .GlobalEnv) 
{
    for (i in 1:length(numbers)) {
        eegout = read.table(paste(filenamebase, numbers[i], ".txt", 
            sep = ""), header = T, skip = 1)
        eeg.subjectname = readLines(paste(filenamebase, numbers[i], 
            ".txt", sep = ""), n = 1)
        eeg.subjectname = gsub("\t", "", eeg.subjectname)
        comment(eegout) = eeg.subjectname
        eegout.name = paste(outname, numbers[i], sep = "")
        assign(eegout.name, eegout, envir = env)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
