\name{grandaverage}
\alias{grandaverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
grandaverage
}
\description{
Function to average ERP objects, i.e. \code{data.frame} with timepoints in rows, Electrodes in columns and amplitude values in each cell.
It is used to compute the grandaverage across several subjects, for plotting purposes.
}
\usage{
grandaverage(base, numbers, env = .GlobalEnv)
}
\arguments{
  \item{base}{
A string indicating the beginning of the name of \code{data.frame} containg the ERP data.
}
  \item{numbers}{
The numbers (indicating different subjects), to be averaged by the function.
}
  \item{env}{
The environment in which \code{grandaverage} is to be evaluated.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a data.frame with the values of subjects whose names are specified by the combination of \code{base} and \code{numbers}.
}

\author{
Giorgio Arcara
}
\note{
The objects keep track of the objects averaged in a \code{comment}.
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
	# compute the average of subjects 1 to 20 for the condition 
	# specified by the string "Exp1_word_subj".
	
	word=grandaverage("Exp1_word_subj", 1:20)
}
}

