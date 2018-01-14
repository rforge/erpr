\name{erplist.sortbytime}
\alias{erplist.sortbytime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove sortbytime in an erplist.
}
\description{
This function is meant to be used for single trial erplist. Sort all erpobj in an erplist according to the 
attribute 'TrialTime', stored in the erplist.
}
\usage{
erplist.sortbytime(erplist=NULL, new.name=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{


  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}, referred to single trials.
}

  \item{new.name}{
Optional: the new name of the objects in the erplist.
}

}

\details{
This function is meaningless only with single trial erplist. It can be used after importing with \code{import.erp}.
To sort the trials, according to TrialTime (and rename, accordingly)
}


\author{
Giorgio Arcara
}


\seealso{
\code{\link{import.erp}}, %\code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{m%ean}} 
}
\examples{

# this is just an example of syntax with NON existing files.
# you should youse on your file (at single trial)

## foo = import.erp("non_ex_", numbers=1:50, fileinfo=TRUE, timeinfo=TRUE)
## foo_sorted = eplist.sortbytime(foo)

}

