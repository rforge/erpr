\name{create.roi}
\alias{create.roi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create electrode region of interests
}
\description{
An utility function to create a factor that collapses other factor levels (typically electrode names) in a new variable vector with the new ROI (region of interest) variable.
}
\usage{
create.roi(datall, electrode="electrode", groups=NULL, roi.levels=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datall}{
a  data frame containing ERP data in long format.
}
  \item{electrode}{
column names of datall containing electrode names.
}
  \item{groups}{
a list containing (in separate vectors) electrode names to create the ROI.\cr E.g. \code{list(c("Fp1", "Fp2"), c("P3", "P4"))}.
}
  \item{roi.levels}{
a vector with the names of the newly created factor variable.\cr E.g. \code{c("Frontopolar", "Parietal")}.
}
}
\details{
All levels of the variable \code{electrode} that are not specified in \code{groups} will be coded as \code{NA} in the returned vector.
}
\value{
The function returns a vector with the new coded ROI variable.
}

\author{
Giorgio Arcara
}


\examples{
\dontrun{
data(ERPsets)

datall=erp.mean(base = "Exp1_word_subj", numbers = 1:20,
 win.ini = 400, win.end = 600, startmsec= -200, endmsec=1500)

datall$caudality=create.roi(datall, "electrode", 
groups=list(c("Fp1", "Fp2"), c("P3", "P4")), 
roi.levels=c("Frontopolar", "Parietal"))

table(datall$caudality, datall$electrode)
}	
}
