\name{create.ROI}
\alias{create.ROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.ROI
}
\description{
Utility function to create a factor that collapse other factor levels (typically electrode names) in a new variable vector with the new ROI variable.
}
\usage{
create.roi(datall, Electrode="Electrode", groups=NULL, roi.levels=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datall}{
the data.frame containing ERP data in long format.
}
  \item{Electrode}{
The column names of datall containing the Electrode names
}
  \item{groups}{
a list containing (in separate vectors) the Electrode names to create the ROI. E.g. \code{list(c("Fp1", "Fp2"), c("P3", "P4"))}.
}
  \item{roi.levels}{
a vector with the names of the new created factor variable. E.g. \code{c("Frontopolar", "Parietal")}.
}
}
\details{
All levels of the variable \code{Electrode} that are not specified in \code{groups} will be codified as \code{NA} in the returned vector.
}
\value{
The function returns a vector with the new codified ROI variable.
}

\author{
Giorgio Arcara
}


\examples{
### BISOGNA CREARE UN DATASET IN FORMATO LONG DA ALLEGARE AL PACCHETTO
\dontrun{
datall=ERPlong
datall$caudality=create.roi(datall, "Electrode", groups=list(c("Fp1", "Fp2"),
 c("P1", "P2")), roi.levels=c("Frontopolar", "Parietal"))
}	
}
