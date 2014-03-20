\name{create.mean}
\alias{create.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.mean
}
\description{
From two sets of paired ERP \code{data.frame} objects the function creates a new set of \code{data.frame} objects with the values obtained by averaging the values of the original sets.
}
\usage{
create.mean(bases, numbers, outname = NULL, envir = .GlobalEnv, out.envir = envir, comment.dat = 1)
}
\arguments{
  \item{bases}{
A vector of strings indicating the beginning of the names of \code{data.frame} objects containing the ERP data to be averaged.
}
  \item{numbers}{
The numbers of subjects to be averaged by the function.
}

  \item{outname}{
the \code{base} of the names of the returned \code{data.frame} objects.
}

  \item{envir}{
The environment in which \code{grandaverage} is to be evaluated.
}
  \item{out.envir}{
The environment in which the objects created are returned.
}
  \item{comment.dat}{
A number indicating from which set of objects specified in the argument \code{bases} the comment of the data.frame is retrieved.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a series of ERP \code{data.frame} objects obtained by averaging the sets specified.
}

\author{
Giorgio Arcara
}

\seealso{
\code{\link{create.diff}}
}
\examples{
\dontrun{
create.mean(c("Exp1_word_subj", "Exp1_nonword_subj"), 1:20, outname="Exp1_average_subj")
}
}
