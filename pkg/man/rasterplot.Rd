\name{rasterplot}
\alias{rasterplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Raster plot of results from \code{mass.t.test}.
}
\description{
This function plot the results of \code{mass.t.test} in the form of raster.
}
\usage{
rasterplot(res, startmsec=NULL, endmsec=NULL, palette.col="jet", lower.thresh=NULL, 
upper.thresh=NULL, zlim="default", cex.axis=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{res}{
The results from a call to \code{mass.t.test}. It could be any of the matrices returned by \code{mass.t.results}
}

\item{startmsec}{
the start time (in ms) of the ERP vector.
}
\item{endmsec}{
the end time (in ms) of the ERP vector.
}

\item{palette.col}{
The color scheme to be used in the plot. Two built-in palette are \code{"jet"} or "heat". Otherwise, you can specify any palette with \code{colorRampPalette}.
}

\item{lower.thresh}{
set a lower threshold to the results to be displayed. Values below this threshold will not be plotted.
}

\item{upper.thresh}{
set a upper threshold to the results to be displayed. Values below this threshold will not be plotted.
}

\item{zlim}{
	the limits of z axis (i.e. colors in the raster plot). If \code{"default"}, then the limits are automatically computed.
}

\item{cex.axis}{
magnification factor for the y axis.
}
\item{\ldots}{
Further arguments to be passed to the call to \code{plot}.
}

}
\details{
This function plot the results of \code{mass.t.test results}
}

\note{
If the function returns this error \code{ Error in plot.new() : figure margins too large }, you may want to increase the size of your graphic device (e.g. quartz(height=10, width=10)), before calling \code{rasterplot}


}
\value{
A raster plot. It also return a list with \code{mypalette} (a function to create the palette used in the plot) and 
\code{zlim}, the actual limits used in the plot.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mass.t.test}}
}
\examples{

\dontrun{

res = mass.t.test("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, 
erplist1=ERPsets, erplist2=ERPsets, paired=T)

quartz(width=10, height=5)
par(mfrow=c(1,2))
rasterplot(res$t.mat, startmsec=-200, endmsec=1500, main = "all t-values", cex.axis=0.7)
rasterplot(res$sig, startmsec=-200, endmsec=1500, main = "significance", cex.axis=0.7)

}


}
