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
topoplot(erpobj, startmsec = -200, endmsec = 1200, win.ini,
                 win.end, exclude = NULL, elec.coord = NULL, projection
                 = "orthographic", palette.col = "jet", palette.steps =
                 50, return.coord = FALSE, zlim = NULL, interpolation =
                 "cubicspline", extrap = TRUE, interp.points = 100,
                 return.notfound = FALSE, mask = TRUE, contour = TRUE,
                 x.rev = FALSE, draw.elec.pos = TRUE, elec.pos.toplot =
                 "all", elec.pos.pch = 19, elec.pos.cex = 1, draw.nose
                 = FALSE, draw.ears = FALSE, draw.elec.lab = TRUE,
                 elec.lab.adj = c(0.5, NA), elec.lab.cex = 1,
                 elec.lab.toplot = elec.pos.toplot, head.col = "black",
                 head.lwd = 1, nose.lwd=NULL, ears.lwd=NULL, 
                 head.radius=1, ampl.radius=1, ...)
}
\arguments{
	\item{erpobj}{
an ERP dataframe with electrodes in columns and timepoints in rows.
}

  \item{startmsec}{
start time (in ms) of the ERP data frame. It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
end time (in ms) of the ERP data frame.
}

 \item{win.ini}{
the initial edge (in ms) of the window on which the topographic map has to be plotted.
}
  \item{win.end}{
the final edge (in ms) of the window on which the topographic map has to be plotted. If equal to win.ini, then the map of a single 
time point is plotted.
}

\item{exclude}{
	a vector of characters indicating the channel names that are excluded from the plotting
}
\item{elec.coord}{
	an optional data frame with electrode coordinates (see Note).
}
\item{projection}{
	the type of electrode projection from 3d to 2d. Two methods available are \code{"orthographic"} and \code{"equalarea"}.
}
\item{palette.col}{
	the color palette. Two default palettes are available: \code{"jet"} or \code{"heat"}. Otherwise a customized palette can be created.
	A palette will be built as an interpolation trough the vector of colors provided.
}
\item{palette.steps}{
	the number of steps in the color palette.
}
\item{return.coord}{
	if \code{TRUE}, the function doesn't create a topoplot, but returns a data frame containing all the built-in electrode coordinates.
}

\item{zlim}{
	the limits of z axis (i.e. colors in the topographic map). If \code{NULL}, then the limits are automatically computed.
}
\item{interpolation}{
	The interpolation between electrodes. Two methods from package \code{akima} are available: \code{"linear"} and \code{"cubicspline"}.
}
\item{extrap}{
	logical. Extrapolate the data outside the convex hull also. This parameter is ignored if interpolation method is \code{"linear"}.
}
\item{interp.points}{
	the number of points to be used for interpolation. This parameter defines the smoothness of the plot.
}
\item{return.notfound}{
	logical. If \code{TRUE}, the function doesn't create a topoplot, but returns a vector with the electrode names in \code{erpobj} not contained in the supplied list of electrodes.
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
	logical. Should the black points be drawn in correspondence with electrode positions?
}
\item{elec.pos.toplot}{
	the names of the electrodes to plot, with a symbol specified in \code{elec.pos.pch}. 
	Default is \code{"all"} and all electrode positions will be plotted.
}
\item{elec.pos.pch}{ 
	the symbol type for electrode position. Default is \code{19} and electrode positions are plotted as black dots.
	
}
\item{elec.pos.cex}{
	The magnification to be used for the electrode position symbols

}
\item{draw.nose}{
	logical. Should a nose be drawn to indicate upper side of the mask?
}
\item{draw.ears}{
 logical. Should ears be drawn? (just aesthetic)
}
\item{draw.elec.lab}{
	logical. Should electrode labels be drawn?
}
\item{elec.lab.adj}{
	a vector with a pair of values indicating horizontal and vertical adjustment of electrode labels (see \code{\link{text}}).
}
\item{elec.lab.cex}{
	The magnification to be used for the electrode labels.	
}
\item{elec.lab.toplot}{
	the electrode labels to plot. As default, the labels for the electrodes specified with the argument \code{elec.pos.toplot} are plotted. 
}
\item{head.col}{
	line color of head and nose.
}
\item{head.lwd}{
	line width of head.
}
\item{nose.lwd}{
	line width of nose. (if not specified \code{head.lwd} will be used)
}
\item{ears.lwd}{
	line width of ears. (if not specified \code{head.lwd} will be used)
}
\item{head.radius}{
	the radius of the circle depicting the head (default is 1).
}
\item{ampl.radius}{
	the radius of the circle depicting the amplitude values (default is 1). Note that in some cases the electrodes could be outside the head radius and values higher than 1 may be necessary.
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
\code{zlim} of the plot. The second item of the list is a vector indicating the palette created by the function (\code{palette}). This last vector with palette colors can be used by the \code{\link{topoplot.palette}} function.
}
\author{
Giorgio Arcara
}
\note{
The topographic plots are created according the following steps. 1) The electrodes are projected from 3d space to 2d space. 2) Voltage values are interpolated (and extrapolated, if \code{extrap=TRUE}) on 2d space (the interpolation methods come from the \code{nterp} function of \code{akima} package). The spline interpolation is NOT a spherical spline interpolation.\cr
The function already contains a built-in list of electrode coordinates, but some names may not be the same in the supplied data frame and in the built-in list (even if the case is ignored in the match). An optional object with electrode coordinates can be supplied. This object should be a data frame containing these four columns: a column labeled \code{"el.name"} with electrode names, three columns labeled \code{"x", "y", "z"} with spherical coordinates of the electrode. \cr
Type \code{topoplot(return.coord=TRUE)} to see the data frame structure. \cr
In the \code{"orthographic"} projection only \code{"x"} and \code{"y"} of electrode coordinates are mantained. The \code{"equalarea"} projection keeps in the 2d projection the area proprortions as in 3d representation.
The \code{\link{topoplot}} function uses the function \code{interp} of package \code{akima}, that is released under a non-commercial use license.
}


\examples{

\dontrun{
if(require(akima)){

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# check if some electrodes are not present in the list 
# and create an object with these electrode names.

notfound=topoplot(word, return.notfound=TRUE)

#make a topoplot excluding not found electrode
topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
win.end=600, exclude=notfound)
}
}

}
