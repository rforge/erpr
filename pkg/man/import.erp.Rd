\name{import.erp}
\alias{import.erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
import ERP data matrices from .txt files
}
\description{
Import a series of .txt files with ERP data.
}
\usage{
import.erp(filenamebase, numbers, outname = "ERP_subj", envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenamebase}{
a string indicating the beginning of the name of the .txt files containing the ERP data.
}
  \item{numbers}{
the numbers (indicating the subjects) of the files to be imported.
}
  \item{outname}{
a string inficating the beginning of the name of the objects that will be created.
}
  \item{envir}{
the environment in which the objects created by \code{import.erp} are stored.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
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
