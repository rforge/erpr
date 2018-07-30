\name{create.mean}
\alias{create.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create ERP data frames with mean values from a series of data frames contained in an erplist.
}
\description{
This function creates a new set of data frames from two sets of paired ERP data frames by averaging the values of the original sets.
}
\usage{
create.mean(bases, numbers, outname = NULL, erplist = NULL, fileinfo = 1)
}
\arguments{
  \item{bases}{
a vector of strings indicating the beginning of the names of the data frames containing the ERP data to be averaged.
}
  \item{numbers}{
the numbers of the subjects to be averaged.
}

  \item{outname}{
the \code{base} of the names of the returned data frames.
}
 
  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}
  \item{fileinfo}{
a number indicating from which set of objects specified in the argument \code{bases} the information of the data frame is retrieved.
}
}

\details{
For more information on \code{fileinfo} argument see Details section of \code{\link{import.erp}}.
}

\value{
A series of ERP data frames obtained by averaging the specified sets.
}

\author{
Giorgio Arcara
}

\seealso{
\code{\link{create.diff}}
}
\examples{

data(erplistExample)

ERPmean=create.mean(c("Exp1_word_subj", "Exp1_nonword_subj"), 1:20,
outname="Exp1_mean_subj", erplist=erplistExample)

erplistExample=c(erplistExample, ERPmean)

}
