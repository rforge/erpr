\name{import.erp}
\alias{import.erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
import ERP data matrices from ASCII files (file extension should be specified).
}
\description{
Import a series of .txt files with ERP data.
}
\usage{
import.erp(filenamebase, numbers, ext=".txt", outname = "ERP_subj", envir = .GlobalEnv, fileinfo=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenamebase}{
a string indicating the beginning of the name of the .txt files containing the ERP data.
}
  \item{numbers}{
the numbers (indicating the subjects) of the files to be imported.
}
  \item{ext}{
A string indicating file extension. Default is ".txt"
}

  \item{outname}{
a string indicating the beginning of the name of the objects that will be created.
}
  \item{envir}{
the environment in which the objects created by \code{import.erp} are stored.
}
\item{fileinfo}{
Optional argument. If \code{TRUE} the function expects that in the first row of imported file there is a string indicating the name of the subject.
}
}
\details{
The optional argument \code{fileinfo} can be used to store some short information on the file. This is usually an ID for the Subject or the name of the original file from which the ASCII file has been exported (for example if the files have been exported from eeglab, the name of the .set file). All objects that will be created from a file imported with \code{import.erp} will keep this information as a \code{comment}. Type \code{comment(objectname)} to access this information. If \code{fileinfo=FALSE}, the name of the file imported will be stored as a comment.
}

\value{
A series of ERP data frames.
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

\examples{
\dontrun{
#import.erp(filenamebase="Exp1_word_subj", numbers=1:20, outname="Exp1_word_subj")
}
}
