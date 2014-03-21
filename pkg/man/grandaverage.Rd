\name{grandaverage}
\alias{grandaverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the grandaverage of ERP data frames for graphical purposes
}
\description{
A function to average ERP objects, i.e. data frames with timepoints in rows, electrodes in columns and amplitude values in each cell.
It is used to compute the grandaverage across several subjects, for plotting purposes.
}
\usage{
grandaverage(base, numbers, electrodes="all", envir = .GlobalEnv, NA.sub=TRUE)
}
\arguments{
  \item{base}{
a string indicating the beginning of the name of the \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of subjects to be averaged.
}

\item{electrodes}{
A character vector indicating the electrodes on which perform the grandaverage. Default is \code{"all"}, 
and the \code{grandaverage} is performed on all electrodes of the data frames.
}
  \item{envir}{
the environment in which \code{grandaverage} is to be evaluated.
}
  \item{NA.sub}{
Logical. If \code{TRUE}, electrodes columns with \code{NA} are ignored in grandaverage computation.
In this case, a Warning is displayed.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
a data frame with the values of subjects specified by the combination of \code{base} and \code{numbers}.
}

\author{
Giorgio Arcara
}
\note{
The function keeps track of the objects averaged in a \code{comment}.
}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
	# compute the average of subjects 1 to 20 for the condition 
	# specified by the string "Exp1_word_subj".
	
	word=grandaverage("Exp1_word_subj", 1:20)
}
}

