\name{topoplot.palette}
\alias{topoplot.palette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a palette to a plot
}
\description{
This function draws a topoplot palette as a rectangle with colored areas indicating the palette shades.
}
\usage{
topoplot.palette(cols, pos = c(0.5,0.5), p.width=0.2, p.height=0.8, 
horizontal = FALSE, rev= FALSE, palette.lwd=1, palette.bord="black", 
palette.lim = c(-5,5), labels=TRUE, lab.dist = 1, lab.cex = 1,
 lab.font = 1, lab.family = "")
}
\arguments{
  \item{cols}{
  	a vector of colors to draw the palette. This vector may be the output of \code{\link{topoplot}}.
}
	\item{pos}{
		the position of the palette as proportion of the plot region. Default (\code{c(0.5, 0.5)}) is centered in the screen.
	}
	\item{p.width}{
		the width of the palette as proportion of the plot region.
	}
	\item{p.height}{
		the height of the palette as proportion of the plot region.
	}
	\item{horizontal}{
		logical. Should the color shades be plotted horizontally (default is vertical).
	}
	\item{rev}{
		logical. Should the palette shades order be reversed?
	}
	\item{palette.lwd}{
		line width of the palette rectangle.
	}
	\item{palette.bord}{
		color of the palette border.
	}
	\item{palette.lim}{
		limits of the palette. They will be plotted as text if \code{labels=TRUE}.
	}
	\item{labels}{
		logical. Should labels indicating \code{zlim} be drawn?
	}
	\item{lab.dist}{
		a number indicating the distance of the labels from the palette rectangle. Positive numbers move the labels from the palette rectangle, whereas negative numbers move the labels toward the rectangle. 
	}
	\item{lab.cex}{
		magnification factor for the labels.
	}
	\item{lab.font}{
		font type for the labels (see \code{\link{par}}).
	}
	\item{lab.family}{
		font family for the labels (see \code{\link{par}}).
	}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
the function draws a palette on an existing plot.
}
\author{
Giorgio Arcara
}
\note{
The two parameters \code{cols} and \code{zlim} may be supplied with values that are returned from \code{\link{topoplot}}.
}


\examples{

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# check if some electrodes are not present in the list
# and create an object with these electrode names.
notfound=topoplot(word, return.notfound=TRUE)

#define a layout for
mat=matrix(c(1,2), 1, 2, byrow=TRUE)

layout(mat, widths=c(0.8, 0.2))

#make a topoplot excluding not found electrode
par(pty="s")
topo.data=topoplot(word, startmsec=-200, endmsec=1500, win.ini=400, 
win.end=600, exclude=notfound)

#draw the palette on a new empty plot.
par(pty="m", mar=c(0,0,0,0))
plot.new()
topoplot.palette(cols=topo.data$palette, 
palette.lim=topo.data$zlim, p.height=0.6)

}
