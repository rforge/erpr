\name{ms.locator}
\alias{ms.locator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	locate ms on a ERP plot
}
\description{
This functions is meant to be used with a plot obtained with \code{\link{erp}} function. It reads the position of the graphics cursor when the (first) mouse button is pressed and returns the x-coordinates in ms. 
	}
\usage{
ms.locator(type = "p", startmsec, endmsec, lengthsegment, label=TRUE, n=512, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
	the type of plot. this parameter is passed to \code{locator}
}

  \item{startmsec}{
     the time (in ms) of the start of the segment.
}
  \item{endmsec}{
     the time (in ms) of the end of the segment.
}
  \item{lengthsegment}{
    the length (in points) of the segment.
}
	\item{label}{logical. If \code{TRUE} the located ms are plotted on the figure.
}
	
	\item{\ldots}{further parameters to be passed to \code{locator} function.
}
	
	\item{n}{The maximum number of points to locate.
}

}

\value{
The time in ms associated with each located point is printed on the \code{R} prompt. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Giorgio Arcara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{locator}
}

\examples{

\dontrun{
	
data(erplistExample)

word=grandaverage("Exp1_word_subj", 1:20, erplist=erplistExample) 

erp(word$F3, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-6,6))


ms.locator(type="p", startmsec=-200, endmsec=1500, lengthsegment)

}

}					
