\name{topoplot}
\alias{topoplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a topographic map of ERP
}
\description{
Plot a topographic map of an ERP on a specific timepoint or averaging the ERP in a time window. The head is represented as a circle.
}
\usage{
topoplot(erpobj, startmsec=-200, endmsec=1200, win.ini, 
win.end,  exclude = NULL, elec.coord=NULL, projection="orthographic", 
palette.col="jet", palette.steps=10, return.coord = FALSE, zlim=NULL, 
interpolation = "cubicspline", extrap = TRUE, interp.points = 500, 
return.notfound=FALSE, mask = TRUE,  contour=TRUE, x.rev=FALSE, 
draw.elec.pos=TRUE,  draw.nose=FALSE, draw.elec.lab=TRUE, 
elec.lab.adj=c(0.5, NA), head.col="black", head.lwd=1, ...)
}
\arguments{
	\item{erpobj}{
an ERP dataframe with electrodes in columns and timepoints in rows.
}

  \item{startmsec}{
the start time (in ms) of the ERP data frame. It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
the end time (in ms) of the ERP data frame.
}

 \item{win.ini}{
the initial edge (in ms) of the window on which the topographic map has to be plotted.
}
  \item{win.end}{
the final edge (in ms) of the window on which the topographic map has to be plotted. If equal to win.end, then the map of a single 
time point is plotted.
}

\item{exclude}{
	a vector of characters indicating the channel names that are excluded from the plotting
}
\item{elec.coord}{
	an optional data frame with electrode coordinates (see Note).
}
\item{projection}{
	the type of projection of electrode from 3d to 2d. The two methods available are \code{"orthographic"} and \code{"equalarea"}.
}
\item{palette.col}{
	the palette for color. Two default palette are available: \code{"jet"} or \code{"heat"}. Otherwise a customized palette can be created.
	A palette will be built as an interpolation trough the vector of colors provided.
}
\item{palette.steps}{
	the number of steps in the color palette.
}
\item{return.coord}{
	if \code{TRUE} the topoplot function is not run and a data frame containing all the built-in electrode coordinates is returned.
}

\item{zlim}{
	the limits of z axis (i.e. colors in the topographic map). If \code{NULL}, then the limits are automatically computed.
}
\item{interpolation}{
	The interpolation between electrodes. Two methods from package \code{\link{akima}} are available: \code{"linear"} and \code{"cubicspline"}.
}
\item{extrap}{
	logical. Extrapolate data also outside the convex hull. This paramter is ignored if interpolation method is \code{"linear"}.
}
\item{interp.points}{
	the number of points to be used for interpolation. This parameter define the smoothness of the plot.
}
\item{return.notfound}{
	logical. If \code{TRUE}, the topoplot is not run and a vector with the electrode names in \code{erpobj} not contained in the supplied list of electrodes is returned.
}
\item{mask}{
	logical. Should a circular mask around topographic map be drawn?
}
\item{contour}{
	logical. Should contour lines be drawn?
}
\item{x.rev}{
	logical. Should the x axis be reversed?
}
\item{draw.elec.pos}{
	logical. Should black points be drawn in correspondence of electrode positions?
}
\item{draw.nose}{
	logical. Should a nose indicating be drawn to indicate upper side of the mask?
}
\item{draw.elec.lab}{
	logical. Should electrode labels of electrodes be drawn?
}
\item{elec.lab.adj}{
	a vector with a pair of values indicating horizontal and vertical adjustment of electrode labels (see \code{\link{text}}).
}
\item{head.col}{
	the line color of head and nose.
}
\item{head.lwd}{
	the line width of head and nose.
}
\item{...}{
	further parameters to be passed to \code{\link{image}}.
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The topographic map is plotted. The function also returns a list of values that can be used by other functions. The first item of the list is a pair of values indicating the 
\code{zlim} of the plot. The second item of the list is a vector indicating the palette create by the function (\code{palette}). This last vector with palette colors can be used by the \code{\link{topoplot.palette}} function.
}
\author{
Giorgio Arcara
}
\note{
The topographic plots are made according the following steps. 1) The electrodes are projected from 3d space to 2d space. 2) Voltage values are interpolated (and extrapolated, if \code{extrap=TRUE}) on 2d space (the interpolation methods come from the \code{\link{interp}} function of \code{\link{akima}} package). The spline interpolation is NOT a spherical spline interpolation.\cr
The function contains already a built-in list of electrode coordinates, but some names may not be the same in the supplied data frame and in the built-in list (even if the case is ignored in the match). An optional object with electrode coordinates can be supplied. This object should be a data frame containing these four columns: a column labeled \code{"el.name"} with electrode names, three columns labeled \code{"x", "y", "z"} with spherical coordinates of the electrode. \cr
Type \code{topoplot(return.coord=TRUE)} to see the data frame structure. \cr
In the \code{"orthographic"} projection only \code{"x"} and \code{"y"} of electrode coordinates are mantained. The \code{"equalarea"} projection keeps in the 2d projection the area proprortions as in 3d representation.
}


\examples{

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# check if some electrodes are not present in the list 
# and create an object with these electrode names.

notfound=topoplot(word, return.notfound=TRUE)

#make a topoplot excluding not found electrode
topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
win.end=600, exclude=notfound)

}
