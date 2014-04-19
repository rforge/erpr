\name{create.diff}
\alias{create.diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create an ERP data frame with differential values
}
\description{
This function creates a new set of data frames from two sets of paired ERP data frames. 
The values are obtained by subtracting the values of the second set from the first set.
All data frames must be in the same erplist.
}
\usage{
create.diff(base1, base2, numbers, outname = NULL, erplist, 
fileinfo = 1)
}
\arguments{
  \item{base1}{
a vector of strings indicating the beginning of the name of the first series of data frames containing ERP data.
}
  \item{base2}{
a vector of strings indicating the beginning of the name of the second series of data frames containing ERP data.
}

  \item{numbers}{
the numbers of the subjects to be subtracted by the function.
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
For more information on the \code{fileinfo} argument, see Details section of \code{\link{import.erp}}.
}
\value{
A series of ERP data frames obtained by subtracting one specified set from the other.
}

\author{
Giorgio Arcara
}

\seealso{
\code{\link{create.mean}}
}
\examples{
\dontrun{
data(ERPsets)
create.diff("Exp1_word_subj", "Exp1_nonword_subj", 1:20,  outname="Exp1_diff_subj")
}
}
