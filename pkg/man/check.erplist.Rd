\name{check.erplist}
\alias{check.erplist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
check consistency of an erplist
}
\description{
The function performs a series of check on an erplist (a list containing ERP data frames according to the rationale of the \code{erpR} package).
}
\usage{
check.erplist(erplist=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{erplist}{
an erplist object containing ERP data frames.
}
}
\details{
The function checks the following errors: 1) check for duplicate data frames; 2) check for data frames with names without \code{number} specified; 3) check for unbalanced combinations of \code{base} and \code{number} 4) check inconsistencies in \code{startmsec} or \code{endmsec} across data.frame in the erplist.For an explanation of the meaning of \code{base} and \code{numbers}, \code{startmsec}, and \code{endmsec} see \code{\link{erpR}}.
}
\value{
The function return the errors found on the erplist.
}

\author{
Giorgio Arcara
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

data(erplistExample)
check.erplist(erplistExample)

}
