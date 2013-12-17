\name{ezresults}
\alias{ezresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ezresults
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ezresults(mod, filter = TRUE, digit = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
%%     ~~Describe \code{mod} here~~
}
  \item{filter}{
%%     ~~Describe \code{filter} here~~
}
  \item{digit}{
%%     ~~Describe \code{digit} here~~
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
function (mod, filter = TRUE, digit = 2) 
{
    if (length(mod) == 1) {
        anovares = mod[[1]]
        res = anovares[anovares[, "p<.05"] == "*", ]
    }
    if (length(mod) > 1) {
        anovares = mod[[1]]
        anovasph = mod[[2]]
        anovacorr = mod[[3]]
        if (filter == TRUE) {
            anovares = anovares[anovares[, "p<.05"] == "*", ]
        }
        anovares[, c(4, 5)] = round(anovares[, c(4, 5)], 2)
        anovares[, 7] = signif(anovares[, 7], digit)
        anovasph = anovasph[rownames(anovasph) \%in\% rownames(anovares), 
            ]
        res = anovares
        if (dim(res)[1] == 0) {
            cat("nessun effetto significativo\n")
        }
        if (dim(res)[1] != 0) {
            res$sphericity.sig = ""
            res[rownames(anovares) \%in\% rownames(anovasph), "sphericity.sig"] = anovasph[, 
                "p<.05"]
            anovacorr = anovacorr[rownames(anovacorr) \%in\% rownames(anovasph), 
                ]
            res$p.corrGG = ""
            res$sig.GG = ""
            res$p.corrHF = ""
            res$sig.HF = ""
            res[rownames(anovares) \%in\% rownames(anovacorr), 
                "p.corrGG"] = signif(anovacorr[, "p[GG]"], digit)
            res[rownames(anovares) \%in\% rownames(anovacorr), 
                "sig.GG"] = anovacorr[, "p[GG]<.05"]
            res[rownames(anovares) \%in\% rownames(anovacorr), 
                "p.corrHF"] = signif(anovacorr[, "p[HF]"], digit)
            res[rownames(anovares) \%in\% rownames(anovacorr), 
                "sig.HF"] = anovacorr[, "p[HF]<.05"]
            return(res)
        }
    }
    if (length(mod) == 1) 
        return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
