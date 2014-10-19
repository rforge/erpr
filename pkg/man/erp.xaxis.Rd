\name{erp.xaxis}
\alias{erp.xaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw an erp x axis
}
\description{
This function plots an x axis for an erp waveform. Numbers and tick positions are determined according to three parameters: \code{length.erp}, \code{startmsec} and \code{endmsec} (the function calls \code{msectopoints}).
}

\usage{
erp.xaxis(length.erp = NULL, startmsec=-200, endmsec = 1200, 
x.tick = seq(-200, 1200, 200), x.labels = x.tick, draw.x.ticks = TRUE,
x.pos = NA, x.outer = FALSE, x.font = NA, x.lty = "solid", x.lwd = 1, 
x.lwd.ticks = 1, x.col = NULL, x.col.ticks = NULL, x.hadj = NA, 
x.padj = NA, x.tcl = -0.5, x.tick.both= FALSE, x.cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{length.erp}{
the length of the ERP vector the axis refers to.
}
\item{startmsec}{
start time (in ms) of the ERP vector.
}
\item{endmsec}{
end time (in ms) of the ERP vector.
}

\item{x.tick}{
the position (in ms) of x ticks.
}

\item{x.labels}{
the labels (usually a vector of numbers) to be plotted on the x axis.
}

\item{draw.x.ticks}{
	a logical value specifying whether x tickmarks and an axis line should be drawn. 
}

\item{x.pos}{
the position of x axis, in terms of y axis coordinates. Default is \code{NA} and the axis is drawn at the bottom of the plot.
}

\item{x.outer}{
a logical value indicating whether the x axis should be drawn in the outer plot margin, rather than the standard plot margin. 
}
\item{x.font}{
font for text of x labels. Default is par("font").	
}
\item{x.lty}{
line type of both the x axis line and the x tick marks.
}
\item{x.lwd}{
line width of the x axis line.	
} 
\item{x.lwd.ticks}{
line width of the x tick marks.
}
\item{x.col}{
colors of the x axis line and the x tick marks.	
}
\item{x.col.ticks}{
the color of the x tick marks (if specified, overrides x.col)
}
\item{x.hadj}{
adjustment (see \code{\link{par}}("adj")) for all x labels parallel ("horizontal") to the reading direction. If this is not a finite value, the default is used (centering for strings parallel to the axis, alignment of the end nearest the axis otherwise).
}
\item{x.padj}{
adjustment for each x tick label purpendicular to the reading direction. For labels parallel to the axes, padj = 0 means right or top alignment, and padj = 1 means left or bottom alignment. This can be a vector given a value for each string, and will be recycled as necessary.
}
\item{x.tcl}{
The length of tick marks as a fraction of the height of a line of text. The default value is  \code{-0.5}.	
}
\item{x.tick.both}{
Logical. Should the tick marks be drawn both above and below the x axis?
}
\item{x.cex}{
The magnification to be used for the x labels.
}
}




%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
An erp x axis is drawn on an existing plot.
}

\author{
Giorgio Arcara
}

\note{
This function is called internally by the \code{\link{erp}} function. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.yaxis}}
}
\examples{

plot(1, ylim=c(-5,5), xlim=c(0,200), frame.plot=FALSE, 
type="n", axes=FALSE, xlab="", ylab="")

erp.xaxis(200, startmsec=-200, endmsec=1500, 
x.pos=0, x.lwd=2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
