\name{erplist.extract}
\alias{erplist.extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract data from an erplist.
}
\description{
This functions extract data from an erplist selecting a subsample of timepoints (i.e., a smaller time window than the original one)
}
\usage{
erplist.extract(bases, numbers, win.ini=NULL, win.end=NULL, point.ini=NULL, point.end=NULL,
erplist=NULL, startmsec=NULL, endmsec=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{bases}{
a vector of chars indicating the beginning of the names of \code{data.frame} containing ERP data. You can specify more than one \code{base}.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}

  \item{win.ini}{
the initial edge (in ms) of the window of the timepoints to extract.
}
  \item{win.end}{
the final edge (in ms) of the window of the timepoints to extract.
}

 \item{point.ini}{
the initial point of the timepoints to extract (overridded in \code{win.ini} or \code{win.end} are specified).
}
  \item{point.end}{
the final point of the timepoints to extract (overridded in \code{win.ini} or \code{win.end} are specified).
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
The function creates a new eplists, with smaller objects then the original. The initial and final timepoints of the new objects are specified (in ms) 
with \code{win.ini} and \code{win.end}
}
\value{
A new erplist with the extracted data.
}

\author{
Giorgio Arcara
}


%\seealso{
%\code{\link{erp.peak}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{m%ean}} 
%}
\examples{

data(ERPsets)

erplist_ext=erplist.extract(base="Exp1_word_subj", numbers=1:20, win.ini=-100, win.end=1200,
erplist=ERPsets, startmsec=-200, endmsec=1500)

# The new erplist now have data that starts from -100 to 1200 ms

}

