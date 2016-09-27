\name{ERPsets}
\alias{ERPsets}
\docType{data}
\title{list of ERP data frames}
\description{
  In the \code{erpR} package, \code{ERPsets} is an \code{erplist}, that is a list of data frames containing ERP data, named according to the rationale of \code{erpR} (see \code{\link{erpR}}). \code{ERPsets} contains 40 data frames, for the combinations of 20 subjects and two experimental conditions. Each data frame has 34 electrodes in columns and 426 timepoints in rows. Data are recorded from -200 ms to 1500 ms. Data were sampled at 250 Hz.
}
\usage{data(ERPsets)}
\format{
	A list of data frames with 426 timepoints (rows) and 34 electrodes (columns).
	}
\source{
Data collected by Giorgio Arcara.
}
\note{some artifacts were added to the original data for didactical purposes.}

\examples{\dontrun{
data(ERPsets)
}}
\keyword{datasets}
