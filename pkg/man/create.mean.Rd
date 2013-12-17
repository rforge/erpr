\name{create.mean}
\alias{create.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.mean
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.mean(bases, numbers, outname = NULL, env = .GlobalEnv, out.env = env, comment.dat = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bases}{
%%     ~~Describe \code{bases} here~~
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
  \item{out.env}{
%%     ~~Describe \code{out.env} here~~
}
  \item{comment.dat}{
%%     ~~Describe \code{comment.dat} here~~
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
function (bases, numbers, outname = NULL, env = .GlobalEnv, out.env = env, 
    comment.dat = 1) 
{
    if (is.null(outname)) {
        stop("devi specificare outname!!")
    }
    for (i in 1:length(numbers)) {
        temp = 0
        for (k in 1:length(bases)) {
            temp = eval(parse(file = "", text = paste(bases[k], 
                numbers[i], sep = "")), env = env) + temp
        }
        temp.out = (temp/length(bases))
        comment(temp.out) = comment(eval(parse(file = "", text = paste(bases[comment.dat], 
            numbers[i], sep = "")), env = env))
        assign(paste(outname, numbers[i], sep = ""), temp.out, 
            env = out.env)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
