\name{grandaverage}
\alias{grandaverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
grandaverage
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grandaverage(base, numbers, env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{numbers}{
%%     ~~Describe \code{numbers} here~~
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
function (base, numbers, env = .GlobalEnv) 
{
    comment_text = paste("Subjects averaged: ", paste(base, numbers[1], 
        sep = ""))
    average.temp = eval(parse(file = "", text = paste(base, numbers[1], 
        sep = "")), env = env)
    for (i in 2:length(numbers)) {
        average.temp = average.temp + eval(parse(file = "", text = paste(base, 
            numbers[i], sep = "")), env = env)
        comment_text = paste(comment_text, paste(base, numbers[i], 
            sep = ""), "\n")
    }
    average = average.temp/length(numbers)
    comment(average) = comment_text
    return(average)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
