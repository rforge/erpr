\name{scalp}
\alias{scalp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scalp
}
\description{
This function plots the waveforms of several ERP electrodes in an rectangular array reflecting the position in the scalp
}
\usage{
scalp(categ, smo = 0.5, label = c("type1"), layout = 1, ylims = "auto", yrev = TRUE, startmsec = -200, endmsec = 1200, lwd = 1, lty = 1, color.list = c("blue", "red", "darkgreen"), legend = F, legend.lab = "default", t.axis = seq(-100, endmsec, 200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ}{
A list indicating the ERP data frame to be plotted. All electrodes specified in the parameter \code{layout} will be plotted. The others will be ignored.
}

  \item{smo}{
The smoothing parameter to be applied before plotting. The default is 0.
}

  \item{label}{
Unused parameter
}
  \item{layout}{
The layout of electrodes to be plotted. Currently only few layout are supported. 
}
  \item{ylims}{
A number indicating the upper and lower limits of the y axis. If "auto", the function calculate the y limits automatically.
}
  \item{yrev}{
Logical. If \code{TRUE} the y axis is plotted with the negative values upward.
}

 \item{startmsec}{
A value, in milliseconds indicating the beginning time of the ERP vector
}
  \item{endmsec}{
A value, in milliseconds indicating the end time of the ERP vector
}

  \item{lwd}{
A vector specifying the line width to be associated to each ERP data frame specfied in \code{categ}
}
  \item{lty}{
A vector specifying the line types to be associated to each ERP data frame specfied in \code{categ}
}
  \item{color.list}{
A vector specifying the colors to be associated to each ERP data frame specfied in \code{categ}
}
  \item{legend}{
Logical. If \code{TRUE} a legend with object names is drawn in the top-right corner of the array. 
}
  \item{legend.lab}{
The legend labels. If "default" names are retrieved from the \code{categ} argument.
}
  \item{t.axis}{
The ticks and sequence of an axis plotted in the bottom-left corner of the array.
}
}

%\details{
%}

\value{
The function plots a map of ERP waveforms in an array resembling the topographic placements of electrodes on the scalp.
}

\author{
Giorgio Arcara
}
\note{
Currently this functions support a very limited number of layouts. Further version will allow to specify customized layouts.
}

\section{Warning }{in the \code{categ} argument, a list must be specified even if the plot is on only one object}

\seealso{
\code{\link{scalp.t}}, \code{\link{scalp.infl}}, \code{\link{erp}}
}
\examples{
####scalp(list(word), smo=0, layout=1, ylim=10)
####scalp(list(word, nonword), smo=0, layout=1, ylim=10, legend=T)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
