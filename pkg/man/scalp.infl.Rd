\name{scalp.infl}
\alias{scalp.infl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
visually inspect the influence of a subject on an average (scalp array)
}
\description{
The function returns a diagnostic plot to investigate the influence of a given ERP data frame on the grandaverage of a series of ERP data frames. A GUI is provided to explore the effect of removing a data frame (that is expected to be associated with a subject) from the grandaverage. Two plots are superposed: the original grandaverage, and the average removing the subject selected via the GUI. The plot shows an array of electrodes with a topography reflecting their positions on the scalp.
}

\usage{
scalp.infl(base, numbers, smo = 0, layout = 3, ylims = 12, 
yrev = FALSE, startmsec = -200, endmsec = 1200, lwd = c(1, 2), 
lty = 1, col = "black", out.col = "red", envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{base}{
a string indicating the beginning of the name of the \code{data.frame} containing the ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged.
}

 \item{smo}{
the smoothing parameter to apply.
}

  \item{layout}{
the layout of electrodes to be plotted. Currently only few layouts are supported. 
}

\item{ylims}{
a number indicating the upper and lower limits of the y axis. If "auto", the function calculates the y limits automatically.
}
  \item{yrev}{
logical. If \code{TRUE}, the y-axis is plotted with the negative values upward.
}

 \item{startmsec}{
the start time (in ms) of the ERP vector
}
  \item{endmsec}{
the end time (in ms) of the ERP vector
}

 \item{lwd}{
line width of the waveforms.
}
  \item{lty}{
line type of the waveforms.
}
  \item{col}{
the color of the grand average including all subjects.
}
  \item{out.col}{
the color of the grand average in which the selected subject is removed
}
  \item{envir}{
the environment in which the ERP data frames are searched for.
}
}
\details{
The function requires the package \code{rpanel}
}

\value{
The function returns a plot showing the influence of a subject on the grandaverage.
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp.infl}}, \code{\link{scalp}}
}
\examples{
\dontrun{
data(ERPdatasets)
# Notice that Subject 1 is clearly particularly influential for the average on OZ.
scalp.infl(base="Exp1_word_subj", numbers=1:20, layout=1, startmsec=-200, endmsec=1500)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
