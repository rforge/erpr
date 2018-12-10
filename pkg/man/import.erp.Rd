\name{import.erp}
\alias{import.erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
import ERP data matrices from ASCII files (file extension should be specified). }
\description{
Import a series of ASCII files with ERP data. Data are imported as a list containing a data frame for each imported file. The field separator character and the character used for decimal points may be specified by the parameters \code{sep} and \code{dec}, that are passed to the function \code{read.table}, called by \code{import.erp}. 

}
\usage{
import.erp(filenamebase, numbers, ext=".txt", 
outname = "ERP_subj", fileinfo=FALSE, abstimeinfo=FALSE, erplist = NULL, path=getwd(), 
electrodes="file", startmsec=NULL, endmsec=NULL, values.multi=1, ...)
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

\item{abstimeinfo}{
If \code{TRUE} the function expects that in the first row of imported file there is a string indicating the absolute trial time in the recording. This is can be crucial to reconstruct the correct order of trials for single-trial analysis.
}


\item{erplist}{
If an erplist is specified, then the imported files will be added to this erplist.
}
\item{path}{
A string indicating the directory where the ASCII files are contained. Otherwise the files will be searched from the current working directory.
}
\item{electrodes}{
if \code{"file"} (the default), electrode names are retrieved from the imported file, as header. Otherwise a vector with electrode names can be specified.
}

\item{startmsec}{
the start time (in ms) of the ERP data frames. It can be a negative value, indicating the baseline time frame.This info will be stored in the imported object as an attribute of the data.frame. 
}

\item{endmsec}{
the end time (in ms) of the ERP data frames.
This info will be stored in the imported object as an attribute of the data.frame. 
}

\item{values.multi}{
A multiplication factor to be applied to all numeric values while importing. Default is 1 (i.e., no correction).
}

\item{\ldots}{
	further parameters to be passed to \code{\link{read.table}}.
}
}
\details{
The optional argument \code{fileinfo} can be used to store some short information on the file. This is usually an ID for the Subject or the name of the original file from which the ASCII file has been exported (for example, if the files have been exported from eeglab, the name of the .set file). All objects that will be created from a file imported with \code{import.erp} will keep this information as a \code{comment}. Type \code{comment(objectname)} to access this information. If \code{fileinfo=FALSE}, the name of the file imported will be stored as a comment.
The \code{timeinfo} option expects that absoulte time of trial (in the recording session), is stored after the string
\code{Time = } (note the spacing).
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
### The following is an example for importing a series of file
## named Exp1_word_subj1.txt, Exp1_word_subj2.txt, etc.

#Exp1 <- import.erp(filenamebase="Exp1_word_subj", numbers=1:20, 
#	outname="Exp1_word_subj", ext=".txt" fileinfo = T, erplist=NULL)
}
}
