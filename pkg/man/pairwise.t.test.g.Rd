\name{pairwise.t.test.g}
\alias{pairwise.t.test.g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pairwise.t.test.g
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pairwise.t.test.g(x1, g.list, id, agg = T, p.adjust.method = p.adjust.methods, pool.sd = !paired, paired = FALSE, alternative = c("two.sided", "less", "greater"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{g.list}{
%%     ~~Describe \code{g.list} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{agg}{
%%     ~~Describe \code{agg} here~~
}
  \item{p.adjust.method}{
%%     ~~Describe \code{p.adjust.method} here~~
}
  \item{pool.sd}{
%%     ~~Describe \code{pool.sd} here~~
}
  \item{paired}{
%%     ~~Describe \code{paired} here~~
}
  \item{alternative}{
%%     ~~Describe \code{alternative} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x1, g.list, id, agg = T, p.adjust.method = p.adjust.methods, 
    pool.sd = !paired, paired = FALSE, alternative = c("two.sided", 
        "less", "greater"), ...) 
{
    g.list.names = deparse(substitute(g.list))
    g.list.names = gsub("\\(", "", g.list.names)
    g.list.names = gsub("\\)", "", g.list.names)
    g.list.names = gsub("^list", "", g.list.names)
    g.list.names = gsub(" ", "", g.list.names)
    g.list[[length(g.list) + 1]] = id
    newdata = aggregate(x1, g.list, mean)
    newdata$newfactor = apply(newdata[1:(length(newdata) - 2)], 
        1, function(x) {
            paste(x, collapse = "_")
        })
    x = newdata$x
    g = newdata$newfactor
    if (paired & pool.sd) 
        stop("Pooling of SD is incompatible with paired tests")
    DNAME <- paste(deparse(substitute(x1)), "and", g.list.names[1])
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    alternative <- match.arg(alternative)
    if (pool.sd) {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            if (alternative == "two.sided") 
                2 * pt(-abs(t.val), total.degf)
            else pt(t.val, total.degf, lower.tail = (alternative == 
                "less"))
        }
    }
    else {
        METHOD <- if (paired) 
            "paired t tests"
        else "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$p.value
        }
        compare.levels.t <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$statistic
        }
        compare.levels.df <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$parameter
        }
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    TVAL <- pairwise.table(compare.levels.t, levels(g), "none")
    DF <- pairwise.table(compare.levels.df, levels(g), "none")
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        t.value = TVAL, df = DF, p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
