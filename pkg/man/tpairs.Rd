\name{tpairs}
\alias{tpairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate pairwise t-tests
}
\description{
An utility function to perform a series of t-tests on several combinations of levels of one or more variables. P-value correction methods can be specified.
}
\usage{
tpairs(dat, vars, contr, dep, wid, p.adjust.methods = "none", 
paired = "CHECK", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a \code{data.frame} including all variables for the t-test computation.
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
p-value correction methods as in \code{\link{p.adjust}}.
}
  \item{paired}{
logical. If TRUE, paired t-tests are performed. You can also specify \code{"CHECK"} and the function check if using paired t-tests or not (see the details)
}

  \item{\dots}{
Arguments passed to \code{t.test}.
}
}
\details{
The \code{contr} argument allows to specify a subset of combinations of levels of the variables listed in \code{vars} to be performed. The contrasts have to be specified as a list of lists. Each list within the main list contains two vectors with strings. The strings specify the levels of the variables in \code{vars} that have to be compared. The convention is to specify the levels separated by an underscore. For example suppose that \code{vars=c("height", "color")}, and\cr \code{contr=list(list(c("high_red"), c("low_red")), list(c("high_green", "low_green")))} will perform two t-tests. The first t-test it will compare the mean for \code{height=="high"} and \code{color=="red"} with the mean of \code{height=="low"}, and \code{color=="red"}. The second t-test will compare the mean of \code{height=="high"}, and \code{color=="green"} with the mean of \code{height=="low"} and \code{color=="green"}. If a p-value correction method is specified, only p-values of the t-test carried out will be taken into account.
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
If \code{paired = "CHECK"} is specified, than the functions will check if all \code{wid} are duplicated. If they are, data are sorted and paired t-tests are performed. If not, between t-tests are performed. This option is useful if all contrasts include both paired and unpaired comparisons.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t.test}}, \code{\link{p.adjust}}
}
\examples{

# simulate some subjects
subjRT=rnorm(20, 500, 100)

#simulate the effects of three experimental conditions for each subject
condA=rnorm(20, 50, 10)
condB=rnorm(20, -40, 10)
condC=rnorm(20, 20, 10)

#create a data frame
dat=data.frame(Subject=rep(1:20,3), 
condition=c(rep("A", 20), rep("B", 20), rep("C", 20)), 
RT=c(subjRT+condA, subjRT+condB, subjRT+condC ))

#perform pairwise t.test
tpairs(dat, "condition", "all", "RT", "Subject", var.equal=TRUE)	


}
