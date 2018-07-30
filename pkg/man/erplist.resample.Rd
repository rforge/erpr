\name{erplist.resample}
\alias{erplist.resample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resample ERP data frames in an erplist.
}
\description{
This function resample all the data of several ERP data frames, stored in a \code{erplist}. It relies on the \code{signal} package.
}
\usage{
erplist.resample(base=NULL, numbers=NULL, erplist=NULL, 
outname=paste(base, "_res", sep=""), sr.factor=c(1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{base}{
a string indicating the beginning of the name of \code{data.frame} containing ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}

  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}

  \item{outname}{
the \code{base} of the name of the resulting ERP data.frames (that will result from the combination of this base and \code{numbers})
}
  
\item{sr.factor}{
A vector of two values, determining the ratio of the resulting resampling. For example the default \code{c(1,2)}, halves the sampling rate. A value of \code{c(1,4)} will change the sampling rate to 1/4 of the original and so on.
}
}

\details{
The function takes an \code{erplist} containing ERP \code{data.frame} as input, in the style of \code{erpR} and use the \code{resample} function from the \code{signal} pacakge to perform resampling. 
}
\value{
A new erplist with the data resampled.
}

\author{
Giorgio Arcara
}
\note{
This function rely on the \code{resample} function from \code{signal} package. Note that you have to install separately the \code{signal} package.
}


%\seealso{
%\code{\link{erp.peak}}, \code{\link{erp.latency}}, \code{\link{rearrange}},  \code{\link{m%ean}} 
%}
\examples{

\dontrun{
data(erplistExample)

erplist_res=erplist.resample(base="Exp1_word_subj", numbers=1:20, 
erplist=erplistExample, sr.factor=c(1,2))

# original data were sampled at 500 Hz, data in erplist_res are sampled at 250 Hz. 
}


}

