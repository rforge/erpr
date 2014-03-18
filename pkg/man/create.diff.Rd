\name{create.diff}
\alias{create.diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.diff
}
\description{
From two sets of paired ERP \code{data.frame} objects the function creates a new set of \code{data.frame} objects with the values obtained by subtracting the values of the second set from the first set.
}
\usage{
create.diff(base1, base2, numbers, outname = NULL, env = .GlobalEnv, out.env = env, comment.dat = 1)
}
\arguments{
  \item{base1}{
A vector of strings indicating the beginning of the name first series of \code{data.frame} objects containg the ERP data.
}
  \item{base2}{
A vector of strings indicating the beginning of the name second series of \code{data.frame} objects containg the ERP data.
}

  \item{numbers}{
The numbers (indicating different subjects), to be subtracted by the function.
}

  \item{outname}{
the \code{base} of the names of the returned \code{data.frame} objects.
}

  \item{env}{
The environment in which \code{grandaverage} is to be evaluated.
}
  \item{out.env}{
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
a series of ERP \code{data.frame} objects obtained by subtracting the sets specified (the first minus the second).
}

\author{
Giorgio Arcara
}

\seealso{
\code{\link{create.mean}}
}
\examples{
dontrun{
create.diff(c("Exp1_word_subj", "Exp1_nonword_subj"), 1:20, outname="Exp1_diff_subj")
}
}
