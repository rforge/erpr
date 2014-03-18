\name{load.eeg.multi}
\alias{load.eeg.multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
load.eeg.multi
}
\description{
inport in the workspace a series of .txt files with ERP data.
}
\usage{
load.eeg.multi(filenamebase, numbers, outname = "ERP_subj", env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenamebase}{
A string indicating the beginning of the name of the .txt files containg the ERP data.
}
  \item{numbers}{
The numbers (indicating different subjects) of the files to be imported.
}
  \item{outname}{
A string inficating the beginning of the name of the objects that will be created on the environment.
}
  \item{env}{
The environment in which the objects created by \code{load.eeg.multi} are stored.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a series of ERP \code{data.frame} objects.
}

\author{
Giorgio Arcara
}
\note{
%%  ~~further notes~~
}



\examples{
load.eeg.multi(filenamebase="Exp1_word_subj", numbers=1:20, outname="Exp1_word_subj")
}