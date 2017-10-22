\name{mass.t.test}
\alias{mass.t.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
perform mass univariate statistics on an erplist.
}
\description{
This function performs a series of t-tests on each timepoint comparing two sets of ERP data frames. The first set is specified by \code{base1} and \code{numbers1}. The second is specified by \code{base2} and \code{numbers2}. Several options are available.
}
\usage{
mass.t.test(base1=NULL, base2=NULL, numbers1=NULL, numbers2=numbers1, startmsec=NULL, 
endmsec=NULL, paired=F, erplist1=NULL, erplist2=erplist1,  electrodes="all", to.exclude=NULL, 
interval=c(startmsec, endmsec), p.adjust.method="none", n.permutations=1000, p.crit=0.05, 
stable.diff=FALSE, crit.msec=NULL, crit.npoints=NULL, neighbours=NULL, na.rm=TRUE)
}
\arguments{	
	\item{base1}{
a string indicating the beginning of the name of the first set of ERP data frames.
}
	\item{base2}{
a string indicating the beginning of the name of the second set of ERP data frames.
}
  \item{numbers1}{
the numbers of the subjects of the first set of ERP data frames.
}
  \item{numbers2}{
the numbers of the subjects of the second set of ERP data frames.
}

 \item{startmsec}{
the start time (in ms) of the ERP vector
}
  \item{endmsec}{
the end time (in ms) of the ERP vector
}

  \item{paired}{
logical. This argument specifies if the t-tests are paired or not.
}

 \item{erplist1}{
a list containing the ERP data frames specified in \code{base1} and \code{numbers1}.
}
  \item{erplist2}{
a list containing the ERP data frames specified in \code{base2} and \code{numbers2}.
}

\item{electrodes}{
A vector with the names of the electrodes that should be influded in the analysis. Default is \code{"all"} and all electrodes will be used.
}

\item{to.exclude}{
A vector with electrode names that should be excluded from the analsysis.
}

  \item{interval}{
a vector with a pair of values indicating the interval (in milliseconds) to be plotted.
}

  \item{p.adjust.method}{
p-value correction methods as in \code{\link{p.adjust}}. Alternatively \code{"tmax"} can be used to perform permutation test as in Groppe et al., 2011.
}

 \item{n.permutations}{
 number of permutation if a permutation test is specified in \code{p.adjust.methods}, (currently only \code{"tmax"} is available) .
 }

  \item{p.crit}{
the alpha of the t-tests. Default is 0.05.
}
  \item{stable.diff}{
Logical (default is \code{FALSE}). If \code{TRUE}, significant results are filtered, and only consecutive sigificant results as specified in \code{crit.msec} or \code{crit.npoints} are retained.
}

  \item{crit.msec}{
if \code{stable.diff=TRUE}, only sequences of consecutive significant timpoints during at least \code{crit.msec} milliseconds will be retained in the results (Overridden if \code{crit.npoints} is specified).
}

  \item{crit.npoints}{
if \code{stable.diff=TRUE}, only sequences of consecutive significant timpoints during at least \code{crit.npoints} number of timepoints will be retained in the results.

}
  \item{neighbours}{
Currently unused.
}
 \item{na.rm}{
 Logical. Should \code{NA} values be removed from the analysis?
 }
}


\details{ 
This function perform a series of t-tests, according to a Mass Univariate Statistics approach (Groppe et al., 2011)
}
\value{
The function returns results in the form of a list containing a series of objects. \code{param}, contains the parameters of the call to \code{mass.t.test}. \code{mass.t.results}, contains the t.values of the results filtered according to the \code{mass.t.results} call. \code{sig} is a logical matrix containing the significant results (0 = not significant, 1 = significant). \code{t.mat} is a matrix containing all the t-values (not filtered). \code{p.mat} is a matrix containing the p.values, unfiltered.
If \code{stable.diff=TRUE} an additional object containing the list of beginning and end of signficant time windows is included in the ouput.
}
\references{
Groppe, D. M., Urbach, T. P., and Kutas, M. (2011). Mass univariate analysis of event-related brain potentials fields I: a critical tutorial review. Psychophysiology, 48(12), 1711-25.
}
\author{
Giorgio Arcara
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.t}}
}
\examples{

\dontrun{
data(ERPsets)

res = mass.t.test("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, 
erplist1=ERPsets, erplist2=ERPsets, paired=T)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
