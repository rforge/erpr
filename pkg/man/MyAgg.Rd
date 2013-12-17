\name{myAgg}
\alias{myAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
myAgg
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myAgg(formula, data, FUN, ..., subset, na.action = na.omit) 

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
  \item{numbers2}{
%%     ~~Describe \code{numbers2} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{env2}{
%%     ~~Describe \code{env2} here~~
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
function (formula, data, FUN, ..., subset, na.action = na.omit) 
{
  if (missing(formula) || !inherits(formula, "formula")) 
    stop("'formula' missing or incorrect")
  if (length(formula) != 3L) 
    stop("'formula' must have both left and right hand sides")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$FUN <- NULL
  m[[1L]] <- as.name("model.frame")
  if (formula[[2L]] == ".") {
    rhs <- unlist(strsplit(deparse(formula[[3L]]), " *[:+] *"))
    lhs <- sprintf("cbind(%s)", paste(setdiff(names(data), 
                                              rhs), collapse = ","))
    lhs
    m[[2L]][[2L]] <- parse(text = lhs)[[1L]]
  }
  mf <- eval(m, parent.frame())
  if (is.matrix(mf[[1L]])) {
    lhs <- as.data.frame(mf[[1L]])
    names(lhs) <- as.character(m[[2L]][[2L]])[-1L]
    myOut <- aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
    colnames(myOut) <- c(names(mf[-1L]), 
                         paste(names(lhs), deparse(substitute(FUN)), sep = "."))
  }
  else {
    myOut <- aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
    colnames(myOut) <- c(names(mf[-1L]), 
                         paste(strsplit(gsub("cbind\\(|\\)|\\s", "", 
                                             names(mf[1L])), ",")[[1]],
                               deparse(substitute(FUN)), sep = "."))
  } 
  myOut
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
