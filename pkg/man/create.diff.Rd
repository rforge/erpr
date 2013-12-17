\name{create.diff}
\alias{create.diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.diff
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.diff(base1, base2, numbers, outname = NULL, env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base1}{
%%     ~~Describe \code{base1} here~~
}
  \item{base2}{
%%     ~~Describe \code{base2} here~~
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
function (base1, base2, numbers, outname = NULL, env = .GlobalEnv) 
{
    if (is.null(outname)) {
        stop("devi specificare outname!!")
    }
    for (i in 1:length(numbers)) {
        assign(paste(outname, numbers[i], sep = ""), (eval(parse(file = "", 
            text = paste(base1, numbers[i], sep = "")), env = env)) - 
            eval(parse(file = "", text = paste(base2, numbers[i], 
                sep = "")), env = env), env = env)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
