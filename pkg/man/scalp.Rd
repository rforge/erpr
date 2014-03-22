\name{scalp}
\alias{scalp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot ERP waveforms in a scalp array
}
\description{
This function plots the waveforms of several electrodes in an rectangular array reflecting their positions on the scalp
}
\usage{
scalp(categ, smo = 0.5, label = c("type1"), layout = 1, 
ylims = "auto", yrev = TRUE, startmsec = -200, endmsec = 1200, 
lwd = 1, lty = 1, color.list = c("blue", "red", "darkgreen"), 
legend = F, legend.lab = "default", t.axis = seq(-100, endmsec, 200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{
a list indicating the ERP data frame to be plotted. All electrodes specified in the parameter \code{layout} will be plotted. The other electrodes will be ignored.
}

  \item{smo}{
the smoothing parameter to be applied before plotting. The default is 0.
}

  \item{label}{
unused parameter
}
  \item{layout}{
the layout of electrodes to be plotted. Currently only few layouts are supported. 
}
  \item{ylims}{
a number indicating the upper and lower limits of the y-axis. If "auto", the function calculates the y limits automatically.
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
a vector specifying the line width to be associated with each ERP data frame specified in \code{categ}
}
  \item{lty}{
a vector specifying the line types to be associated with each ERP data frame specified in \code{categ}
}
  \item{color.list}{
a vector specifying the colors to be associated with each ERP data frame specified in \code{categ}
}
  \item{legend}{
logical. If \code{TRUE} a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
the legend labels. If "default", the names are retrieved from \code{categ}.
}
  \item{t.axis}{
the position of the ticks on an additional axis indicating the time. This axis is plotted in the bottom-left corner of the plot array.
}
}

%\details{
%}

\value{
The function plots a map of ERP waveforms in an array resembling the topographic locations of the electrodes on the scalp.
}

\section{Warning}{
In \code{categ} argument, a list must be specified even if only one object is plotted.
}

\author{
Giorgio Arcara
}
\note{
Currently this function supports a very limited number of layouts. Further versions will allow to specify customized layouts.
}

\section{Warning }{in the \code{categ} argument, a list must be specified even if the plot is on only one object}

\seealso{
\code{\link{scalp.t}}, \code{\link{scalp.infl}}, \code{\link{erp}}
}
\examples{
\dontrun{
word=grandaverage("Exp1_word_subj", 1:20)

nonword=grandaverage("Exp1_nonword_subj", 1:20)

scalp(list(word), smo=0, layout=1, ylim=10)

scalp(list(word, nonword), smo=0, layout=1, ylim=10, legend=T)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
