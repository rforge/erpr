\name{ERPsets}
\alias{ERPsets}
\docType{data}
\title{ERP data frames}
\description{
  40 data frames for 20 subjects and two experimental conditions. Each data frame has electrodes in columns and timepoints in rows. Data are recorded from -200 ms to 1500 ms.
}
\usage{data(ERPsets)}
\format{
	A data frame with 426 timepoints (rows) and 34 electrodes (columns).
	\describe{
  	\item{\code{Fp1, ..., HEOG1}}{Amplitude values at given electrodes.}
     }
}
\source{
Data collected by Giorgio Arcara.
}
\examples{\dontrun{
data(ERPsets)
}}
\keyword{datasets}
