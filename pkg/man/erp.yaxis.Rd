\name{erp.yaxis}
\alias{erp.yaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw an erp y axis
}
\description{
This function plots an y axis of an erp waveform. The position of the axis is determined according to three parameters: \code{length.erp}, \code{startmsec} and \code{endmsec} (the function calls \code{msectopoints}). 
}

\usage{
erp.yaxis(length.erp = NULL, startmsec=NULL, endmsec = NULL, y.tick=seq(-6,6,2), 
y.labels = y.tick, y.las=0, draw.y.ticks = TRUE, y.pos = NA,  y.outer = FALSE, 
y.font = NA, y.lty = "solid", y.lwd = 1, y.lwd.ticks = 1, y.col = NULL, 
y.col.ticks = NULL, y.hadj=NA, y.padj = NA, y.tcl = -0.5, y.tick.both= FALSE, 
y.cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{length.erp}{
the length of the ERP vector the axis is referred to.
}
\item{startmsec}{
the start time (in ms) of the ERP vector.
}
\item{endmsec}{
the end time (in ms) of the ERP vector.
}
\item{y.labels}{
the labels (usually a vector of numbers) to be plotted on the y axis.
}
\item{y.las}{ 
the style of y axis labels.
}
\item{draw.y.ticks}{
a logical value specifying whether y tickmarks and an axis line should be drawn. 
}
\item{y.pos}{
the position of y axis, in terms of x axis coordinates (in ms). Default is \code{NA} and the axis is drawn at the left of the plot.
}
\item{y.tick}{
the position (in ms) of y ticks.
}
\item{y.outer}{
a logical value indicating whether the y axis should be drawn in the outer plot margin, rather than the standard plot margin. 
}
\item{y.font}{
font for the text of y labels. Default is par("font").	
}
\item{y.lty}{
line type for both the y axis line and the y tick marks.
}
\item{y.lwd}{
line width for the y axis line.	
} 
\item{y.lwd.ticks}{
line width for the y tick marks.
}
\item{y.col}{
colors for the y axis line and the y tick marks.	
}
\item{y.col.ticks}{
color for the y tick marks (if specified, overrides y.col)
}
\item{y.hadj}{
adjustment (see \code{\link{par}}("adj")) for all y labels parallel ("horizontal") to the reading direction. If this is not a finite value, the default is used (centring for strings parallel to the axis, alignment of the end nearest the axis otherwise).
}
\item{y.padj}{
adjustment for each y tick label purpendicular to the reading direction. For labels parallel to the axes, padj = 0 means right or top alignment, and padj = 1 means left or bottom alignment. This can be a vector given a value for each string, and will be recycled as necessary.
}
\item{y.tcl}{
The length of tick marks as a fraction of the height of a line of text. The default value is  \code{-0.5}.	
}
\item{y.tick.both}{
Logical. Should the tick marks be drawn both above and below the y axis?
}
\item{y.cex}{
The magnification to be used for the y labels.
}
}




%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
An erp y axis is drawn on an existing plot.
}

\author{
Giorgio Arcara
}

\note{
This function is called internally by the \code{\link{erp}} function. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.xaxis}}
}
\examples{

plot(1, ylim=c(-5,5), xlim=c(0,200), frame.plot=FALSE, 
type="n", axes=FALSE, xlab="", ylab="")

erp.yaxis(200, startmsec=-200, endmsec=1500, y.pos=0, 
y.lwd=2, y.tick=seq(-4,4,2))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
