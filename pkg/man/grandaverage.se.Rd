\name{grandaverage.se}
\alias{grandaverage.se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the standard error of grandaverage of ERP data frames. This function can be used to add standard error bands to ERP plots.
}
\description{
A function to average ERP objects, i.e. data frames with timepoints in rows, electrodes in columns and amplitude values in each cell.
It is used to compute the grandaverage.se across several subjects, for plotting purposes.
}
\usage{
grandaverage.se(base, numbers, electrodes="all", erplist = NULL, NA.sub=TRUE, type="se")
}
\arguments{
  \item{base}{
a string indicating the beginning of the name of the \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of subjects to be averaged.
}

\item{electrodes}{
a character vector indicating the electrodes on which perform the grandaverage.se. Default is \code{"all"}, 
and the \code{grandaverage.se} is performed on all electrodes of the data frames.
}
  \item{erplist}{
a list containing ERP data frames.
}
  \item{NA.sub}{
 In this case, a Warning is displayed.
}
\item{type}{currently the only type available is \code{se}, future version will include other possibilities.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A data frame with the standard error of the mean calculated from the subjects specified by the combination of \code{base} and \code{numbers}.
}

\author{
Giorgio Arcara
}
\note{
The function keeps track of the name of the objects used to calculated standard error in a \code{comment}.
}

\seealso{
\code{\link{grandaverage}},\code{\link{grandaverage.se}}
}
\examples{

data(ERPsets)
# compute the average of subjects 1 to 20 for the condition 
# specified by the string "Exp1_word_subj".
	
word=grandaverage.se("Exp1_word_subj", 1:20, erplist=ERPsets)

}

