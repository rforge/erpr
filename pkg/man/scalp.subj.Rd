\name{scalp.subj}
\alias{scalp.subj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
inspect the ERP of each subject (scalp array)
}
\description{
The function allows the quick inspection of the ERP waveform of of all specified subjects. A GUI is provided to switch across subjects 
}

\usage{
scalp.subj(base, numbers, smo = NULL, layout = 1, outnumber=1,
ylims = 12,  yrev = FALSE, startmsec = NULL, endmsec = NULL, 
lwd = c(1, 2), lty = 1, col = "black", erplist = NULL,
t.axis=NULL, scalp.array=NULL)
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
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}

  \item{layout}{
A number indicating the layout of electrodes to be plotted. Currently only few default layouts are supported (a number from 1 to 5).
Alternatively, a character vector specifying the layout may be supplied (see \code{Details}). 
}

\item{outnumber}{
The number of the subject that will be initially removed from the averaged (this value could be changed also via the GUI).
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

  \item{erplist}{
a list containing ERP data frames.
}
  \item{t.axis}{
the position of the ticks on an additional axis indicating the time. This axis is plotted in the bottom-left corner of the plot array.
}
	\item{scalp.array}{
This argument is vector of two values indicating the number of panels of the plot, as number of rows and number of columns respectively. If a default layout is specified, the number of panels is automatically computed. See also \code{Details}.
	}
}

\details{ The layout parameter can be a number (from 1 to 5), that specifies some default electrode arrangements (currently only a few default arrangements are supported). Otherwise, the argument may be a character vector indicating the names of the electrodes to be plotted. The plot is divided in a number of panels as specified in \code{scalp.array}. The names specified will be plotted from top to bottom and from left to right. If \code{"blank"} is specified an empty panel will be plotted. If \code{"xaxis"} is specified a x axis indicating time will be plotted. If \code{"yaxis"} is specified a y axis indicating voltage will be plotted. 
}

\value{
The function returns a plot of a scalp array showing the influence of a subject on the grandaverage.
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

data(ERPsets)

# Notice that Subject 1 has some problems, especially in OZ
\dontrun{
scalp.subj(base="Exp1_word_subj", numbers=1:20, layout=1, 
startmsec=-200, endmsec=1500, erplist=ERPsets)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
