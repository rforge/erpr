\name{topo.scalp}
\alias{topo.scalp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot ERP waveforms in a scalp array with topographic position
}
\description{
This function plots the waveforms of several electrodes in a topographic array reflecting their positions on the scalp
}
\usage{
topo.scalp(erpdfs, smo=NULL, ylims="auto", yrev=TRUE, startmsec=NULL, endmsec=NULL, lwd=1, lty=1, 
color.list=c("blue", "red", "darkgreen"), legend=F, legend.lab="default", t.axis=NULL,
elec.coord=NULL, el.toplot=NULL, x.margin=1, y.margin=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{erpdfs}{
a list indicating the ERP data frame to be plotted. All electrodes specified in the parameter \code{layout} will be plotted. The other electrodes will be ignored.
}

  \item{smo}{
the smoothing parameter to apply (see \code{\link{smooth.spline}}). Default is \code{NULL} and no smoothing is applied.
}

  \item{ylims}{
a number indicating the upper and lower limits of the y-axis. If "auto", the function calculates the y limits automatically.
}
  \item{yrev}{
logical. If \code{TRUE}, the y-axis is plotted with the negative values upward.
}

 \item{startmsec}{
the start time (in ms) of the erpdfs.
}
  \item{endmsec}{
the end time (in ms) of the ERP erpdfs.
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
logical. If \code{TRUE} a legend with object names is drawn in the top-right corner of the array (currently  not supported). 
}
  \item{legend.lab}{
the legend labels. If "default", the names are retrieved from \code{erpdf} (currently not supported).
}
  \item{t.axis}{
the position of the ticks on an additional axis indicating the time. This axis is plotted in the bottom-left corner of the plot array. Default is from startmsec to endmsec with steps of 200 ms.
}
	\item{elec.coord}{The coordinates of electrodes to be plotted. If \code{NULL} (the default), the electrode positions are retrieved from \code{\link{topoplot}} function.
	}
	\item{el.toplot}{A vector with the names of the electrodes to be plotted.
	}
	\item{x.margin}{A parameter to set the size of xlim in the plot. Default is 1 and larger values make a bigger plot on x-axis.
	}
	\item{y.margin}{A parameter to set the size of xlim in the plot. Default is 1 and larger values make a 
	bigger plot on y-axis.
	}
		\item{\ldots}{Further parameters to be passed to the \code{erp} function, called within \code{topo.scalp}
	}
}

\details{Electrode position are by default retrieved from \code{\link{topoplot}}, function (see \code{\link{topoplot}} for details on how to supply other electrode locations). Electrodes are plotted on a 2D surface using the equalarea projection.
}

\value{
The function plots a map of ERP waveforms in an array reflecting the topographic locations of the electrodes on the scalp.
}


\author{
Giorgio Arcara
}
\note{
Currently this function supports a  limited number of electrode locations. Some electrodes may not be shown because of this limitation.
}


\seealso{
\code{\link{topoplot}}
}
\examples{

data(erplistExample)

word=grandaverage("Exp1_word_subj", 1:20, erplist=erplistExample)

nonword=grandaverage("Exp1_nonword_subj", 1:20, erplist=erplistExample)

topo.scalp(list(word), layout=1, ylim=10)

topo.scalp(list(word, nonword), layout=1, ylim=10, legend=TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
