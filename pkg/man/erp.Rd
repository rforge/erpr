\name{erp}
\alias{erp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot an ERP waveform
}
\description{
A function to plot an ERP waveform of a single electrode. If used with default values the function allows a fast plotting. Several parameters are available for plotting paper quality plots.
}
\usage{
erp(erpdf, electrode, startmsec=NULL, endmsec = NULL, smo = NULL, interval = c(startmsec, endmsec),
xlim = NULL, ylim = NULL, col = "black",  lwd = 1, xlab = "", ylab = "", 
main = "", frame.plot = TRUE, draw.xaxis = TRUE, draw.yaxis = TRUE, type = "l", 
x0 = TRUE, x.tick = NULL, x.labels=x.tick, x.las=0, x.pos = NA,  x.outer = F, 
x.font = NA,  x.lty = "solid", x.lwd = 1, x.lwd.ticks = 1,  x.col = NULL,  
x.col.ticks = NULL,  x.hadj = NA, x.padj = NA, x.tcl = -0.5,  x.tick.both= FALSE,
x.cex = 1, y.tick = NULL,  y.labels = y.tick, y.las=0, y.pos = NA, y.outer = F, 
y.font = NA, y.lty = "solid", y.lwd = 1,  y.lwd.ticks = 1, y.col = NULL, 
y.col.ticks = NULL, y.hadj = NA, y.padj = NA,  y.tcl = -0.5, y.tick.both= FALSE,
 y.cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{erpdf}{
a data.frame with timepoints in row and electrodes in columns.
}
  \item{electrode}{
character indicating the electrode to plot.
}
\item{startmsec}{
the start time (in ms) of the \code{erpdf}.
}
\item{endmsec}{
the end time (in ms) of the \code{erpdf}.
}
  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}

  \item{interval}{
a vector with a pair of values indicating the interval (in milliseconds) to be plotted.
}
\item{xlim}{
a vector with a pair of values indicating the limits (in milliseconds) of the x axis. Default is \code{NULL} and the xlim is automatically computed.
}
\item{ylim}{
a vector with a pair of values indicating the limits (in milliseconds) of the y axis.Default is \code{NULL} and the ylim is automatically computed.
}
\item{col}{
the color of the ERP waveform.
}
\item{lwd}{
the line width of the ERP waveform.
}
\item{xlab}{
	the label of the x axis.
}
\item{ylab}{
	the label of the y axis.
}
\item{main}{
	the title of the plot.
}
\item{frame.plot}{
	a logical indicating whether a box should be drawn around the plot.
}
\item{draw.xaxis}{
	a logical indicating whether the x axis should be drawn.
  }
\item{draw.yaxis}{
	a logical indicating whether a box should be drawn around the plot.
}
\item{type}{
	1-character string giving the type of plot desired. Default is \code{"l"} and a line will be plotted. If "n" the ERP waveform is not plotted (see \code{\link{plot.default}}).
	}
\item{x0}{
	a logical indicating whether a short segment should be drawn to indicate the 0 point (in millisecond)
}
\item{x.tick}{
the position (in milliseconds) of x ticks. Default is \code{NULL} and the ticks are automatically determined.

}

\item{x.labels}{
the labels (usually a vector of numbers) to be plotted on the x axis.
}

\item{y.las}{
the style of y axis labels.

}

\item{x.las}{
the style of x axis labels.
}

\item{x.pos}{
the position of x axis, in terms of y axis coordinates. Default is \code{NA} and the axis is drawn at the bottom of the plot.
}

\item{x.outer}{
a logical value indicating whether the x axis should be drawn in the outer plot margin, rather than the standard plot margin. 
}
\item{x.font}{
font for text of x labels. Defaults to par("font").	
}
\item{x.lty}{
line type for both the x axis line and the x tick marks.
}
\item{x.lwd}{
line width for the x axis line.	
} 
\item{x.lwd.ticks}{
line width for the x tick marks.
}
\item{x.col}{
colors for the x axis line and the x tick marks.	
}
\item{x.col.ticks}{
color for the x tick marks (if specified, override x.col)
}
\item{x.hadj}{
adjustment (see \code{\link{par}}("adj")) for all x labels parallel ("horizontal") to the reading direction. If this is not a finite value, the default is used (centring for strings parallel to the axis, justification of the end nearest the axis otherwise).
}
\item{x.padj}{
adjustment for each x tick label perpendicular to the reading direction. For labels parallel to the axes, padj = 0 means right or top alignment, and padj = 1 means left or bottom alignment. This can be a vector given a value for each string, and will be recycled as necessary.
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
\item{y.tick}{
the position (in milliseconds) of y ticks. Default is \code{NULL} and the ticks are automatically determined.

}

\item{y.labels}{
the labels (usually a vector of numbers) to be plotted on the y axis.
}

\item{y.pos}{
the position of y axis, in terms of x axis coordinates. Default is \code{NA} and the axis is drawn at the left of the plot.
}


\item{y.outer}{
a logical value indicating whether the y axis should be drawn in the outer plot margin, rather than the standard plot margin. 
}
\item{y.font}{
font for teyt of y labels. Defaults to par("font").	
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
color for the y tick marks (if specified, override y.col)
}
\item{y.hadj}{
adjustment (see \code{\link{par}}("adj")) for all y labels parallel ("horizontal") to the reading direction. If this is not a finite value, the default is used (centring for strings parallel to the axis, justification of the end nearest the axis otherwise).
}
\item{y.padj}{
adjustment for each y tick label perpendicular to the reading direction. For labels parallel to the ayes, padj = 0 means right or top alignment, and padj = 1 means left or bottom alignment. This can be a vector given a value for each string, and will be recycled as necessary.
}
\item{y.tcl}{
The length of tick marks as a fraction of the height of a line of teyt. The default value is  \code{-0.5}.	
}
\item{y.tick.both}{
Logical. Should the tick marks be drawn both above and below the y axis?
}
\item{y.cex}{
The magnification to be used for the y labels.
}
\item{\ldots}{
	further parameters to be passed to \code{\link{plot}}.
}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
the plot of an ERP waveform. 
}

\author{
Giorgio Arcara
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp.t}}, \code{\link{erp.infl}},  \code{\link{erp.cor}}, \code{\link{erp.xaxis}}, \code{\link{erp.yaxis}}
}

\examples{

data(erplistExample)

word=grandaverage("Exp1_word_subj", 1:20, erplist=erplistExample) 

erp(word, "F3", smo=0, col="blue",  ylim=c(-6,6))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
