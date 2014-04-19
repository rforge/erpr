\name{import.erp}
\alias{import.erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
import ERP data matrices from ASCII files (file extension should be specified). Data are imported as a list containing a data frame for each files imported.
}
\description{
Import a series of .txt files with ERP data.
}
\usage{
import.erp(filenamebase, numbers, ext=".txt", 
outname = "ERP_subj", fileinfo=FALSE, erplist = NULL, path=getwd())
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

\item{fileinfo}{
If \code{TRUE} the function expects that in the first row of imported file there is a string indicating some information on the subject to be stored as a comment in the data frame created.
}
\item{erplist}{
If an erplist is specified, then the imported files will be added to this erplist.
}
\item{path}{
A string indicating the directory where the ASCII files are contained. Otherwise the files will be searched from the current working directory.
}
}
\details{
The optional argument \code{fileinfo} can be used to store some short information on the file. This is usually an ID for the Subject or the name of the original file from which the ASCII file has been exported (for example, if the files have been exported from eeglab, the name of the .set file). All objects that will be created from a file imported with \code{import.erp} will keep this information as a \code{comment}. Type \code{comment(objectname)} to access this information. If \code{fileinfo=FALSE}, the name of the file imported will be stored as a comment.
}

\value{
A list containing several ERP data frames (one for each file imported). 
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

\examples{
\dontrun{
#Exp1 <- import.erp(filenamebase="Exp1_word_subj", numbers=1:20, outname="Exp1_word_subj", fileinfo = T)
}
}
