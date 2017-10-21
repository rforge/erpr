\name{erplist.offset}
\alias{erplist.offset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove offset in an erplist.
}
\description{
Remove offset (i.e. perform baseline correction) on all the objects of an erplist.
}
\usage{
erplist.offset(base, numbers, win.ini, win.end,
erplist=NULL, startmsec=NULL, endmsec=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of \code{data.frame} containing ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
  \item{win.ini}{
the initial edge (in ms) of the window on which the offset should be calculated.
}
  \item{win.end}{
the final edge (in ms) of the window on which the offset should be calculated.
}

  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}

 \item{startmsec}{
the start time (in ms) of the ERP vector
}
  \item{endmsec}{
the end time (in ms) of the ERP vector
}

}

\details{
The function perform a DC offset (or baseline) correction in all he objects of an erplist.
It simply calcualte (separately for each object and each electrode), the mean value in the window specified 
with \code{win.ini}, and \code{win.end} and subtract from all the vector.
}
\value{
A new erplist with the data modified according to the offset correction.
}

\author{
Giorgio Arcara
}


%\seealso{
%\code{\link{erp.peak}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{m%ean}} 
%}
\examples{

data(ERPsets)

erplist_res=erplist.offset(base="Exp1_word_subj", numbers=1:20,
win.ini=-200, win.end=0, erplist=ERPsets, startmsec=-200, endmsec=1500)

# original data are now baseline corrected according to the time window going from -200 to 0.


}

