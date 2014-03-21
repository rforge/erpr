\name{tpairs}
\alias{tpairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate pairwise t-tests
}
\description{
AN utility function to perform a series of t-tests on several combinations of levels of one or more variables. P-value correction methods can be specified.
}
\usage{
tpairs(dat, vars, contr, dep, wid, p.adjust.methods = "none", 
paired = FALSE, type = "t.test", vars.bet = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
the \code{data.frame} including all variables for the t-test computation.
}
  \item{vars}{
a vector of characters indicating the names of the independent variables to be considered for the t-tests. If length vars > 1, a new variable will be created by combining the levels of the variables specified in \code{vars}.
}
  \item{contr}{
a list specifying the t-test to be performed. If \code{"all"} is specified, t-tests on all possible combinations are performed. See \code{Details} for further specification information. 
}
  \item{dep}{
a string specifying the name of the dependent variable (a column in \code{dat}).
}
  \item{wid}{
optional: a string specifying the variable that encodes the repeated measure identifier.
}
  \item{p.adjust.methods}{
p-value correction methods.
}
  \item{paired}{
logical. If TRUE, paired t-tests are performed.
}
  \item{type}{
The type of statistical test. Currently supported only \code{t.test}
}
  \item{vars.bet}{
Currently unsupported. Specify if there are between-observation variables.
}
  \item{\dots}{
Arguments to be passed to \code{t.test}.
}
}
\details{
The \code{contr} argument allows to specify a subset of combinations of levels of the variables listed in \code{vars} to be performed. The contrasts have to be specified as a list of lists. Each list within the main list contains two vectors with strings. The strings specify the levels of the variables in \code{vars} that have to be compared. The convention is to specify the levels sperated by an underscore. For example suppose that \code{vars=c("height", "color")}, and \code{contr=list(list(c("high_red"), c("low_red")), list(c("high_green", "low_green")))} will perform two t-tests. The first t-test it will compare the mean for \code{height=="high"} and \code{color=="red"} with the mean of \code{height=="low"}, and \code{color=="red"}. The second t-test will compare the mean of \code{height=="high"}, and \code{color=="green"} with the mean of \code{height=="low"} and \code{color=="green"}. If a p-value correction method is specified, only p-values of the t-test carried out will be taken into account.
}
\value{
A \code{data.frame} with the results of the t-tests specified.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}
\note{
The means for the combinations of the variables taken into account are computed internally by \code{tpairs}, according to the specification in \code{vars}, \code{dep}, \code{wid}.
The function is not able to deal with mixed within and between t-tests. Please, perform the two kinds of t-tests separately and take into consideration if the variables specified in \code{vars} are mixed (both within and between). 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t.test}}
}
\examples{
	
#####
	
}
