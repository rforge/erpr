\name{erp.fun.cycle}
\alias{erp.fun.cycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
loop over different files to calculate an ERP measure }
\description{
This functions allow to quickly perform loops over several objects with different \code{base} or different time windows, to calculate an erp measure (i.e., \code{erp.mean}, \code{erp.peak}, or \code{erp.latency})
}
\usage{
erp.fun.cycle(bases=NULL, numbers=NULL, erp.fun=NULL, intervals=NULL, conditions.names=NULL, 
erplist=NULL, startmsec=NULL, endmsec=NULL, others=NULL, intervals.var=TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bases}{
a string indicating the beginning of the name of the object the ERP data (stored in an \code{erplist}.
}
  \item{numbers}{
the numbers (indicating the subjects) of the files to be imported.
}
  \item{erp.fun}{
the name of the function to be used (no quotes, e.g, \code{erp.fun = erp.mean})
}

\item{intervals}{
A list of pair of values, indicating the beginning and end of time windows on which to apply the \code{erp.fun}.
(e.g., \code{list(c(100, 200), c(300,500))})
}

  \item{conditions.names}{
A vector of characters (same length of \code{bases}) that will be the name of the condition in the resulting object.
}

\item{erplist}{
If an erplist is specified, then the imported files will be added to this erplist.
}

  \item{startmsec}{
the start time (in ms) of the ERP data frames. It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
the end time (in ms) of the ERP data frames.
}

  \item{others}{
the names of other variables to be included in the returned data frame. Ignored if the format is "wide".
}
 \item{intervals.var}{
 logical. Should a variable indicating intervals (with characters) should be included?
 }
 \item{\ldots}{
 further arguments to be passed to the \code{erp.fun} specified.
 }



}
\details{
This function calculates an ERP measure on several ERP data frames, stored in a single \code{erplist}. See the help for the used \code{erp.fun} (i.e.,\code{erp.mean}, \code{erp.peak}, or \code{erp.latency}) for further details.
}

\value{
An ERP \code{data.frame} with the result of the call to the \code{erp.fun}
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{erp.mean}}, \code{\link{erp.peak}}, \code{\link{erp.latency}}
}

\examples{

data(ERPsets)

datall=erp.fun.cycle(bases=c("Exp1_word_subj", "Exp1_nonword_subj"), numbers=1:20, erp.fun=erp.mean, 
conditions.names=c("word", "nonword"), erplist=ERPsets, intervals.var=TRUE,
intervals=list(c(100, 200), c(300, 500)), startmsec=-200, endmsec=1500)

}
