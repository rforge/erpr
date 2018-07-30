\name{butterfly}
\alias{butterfly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ERP butterfly plot
}
\description{
A butterfly plot of several ERP data frames.
}
\usage{
butterfly(base, numbers,  electrode = NULL, startmsec = NULL, 
endmsec = NULL, erplist = NULL, outline=NULL, out.col="black", 
add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{base}{
a string indicating the beginning of the name of the data frame containing ERP data.
}
  \item{numbers}{
the numbers of the subjects to be averaged by the function.
}
 \item{electrode}{
 the electrode to be plotted.
 }

 \item{startmsec}{
the start time (in ms) of the ERP data frames. 
It can be a negative value, indicating the baseline time frame.
}
  \item{endmsec}{
the end time (in ms) of the ERP data frames.
}

  \item{erplist}{
a list containing the ERP data frames specified in \code{base} and \code{numbers}.
}

  \item{outline}{
the number of the subject to be outlined (it will be plotted with lwd=2).
}
  \item{out.col}{
the color of the outlined ERP waveform.
}
  
  \item{add}{
add the current plot to a previous plot
}

\item{...}{
	further parameters to be passed to \code{link{erp}}.
}

}
%\details{
%%  ~~ If necessary, more details than in the description above ~~
%}
\value{
A butterfly plot.
}

\author{
Giorgio Arcara
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{erp}}, \code{\link{erp.infl}}, \code{\link{scalp.infl}}
}
\examples{

data(erplistExample)
butterfly(base="Exp1_nonword_subj", numbers=1:20, electrode="Fp1", smo=0, 
erplist=erplistExample, outline=1, ylim=c(-20,20), out.col="red")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
