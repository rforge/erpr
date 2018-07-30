\name{spatial_neighbours}
\alias{spatial_neighbours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 calculate spatianl neighbours. Internal function for \code{mass.t.test}, cluster based permutation
}
\description{
No description
}
\usage{
spatial_neighbours(chanlocs, max_dist, head_radius=NULL)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{chanlocs}{
channel locations
}
  \item{max_dist}{
  maximum  distance between electrodes to consider they are neighbours
  }
  \item{head_radius}{
  default is NULL
  }
}
\details{
This is an R translation of spatial_neighbours.m function in MASS univariate toolbox
}


\value{
  An electrodes by electrodes matrix with channel neighbours (1 = neighbouring, 0 = not neighouring).
}

\author{
  Giorgio Arcara
}

%\note{
  %%  ~~further notes~~
    %}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  %\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
      %}

\examples{
  
# NO EXAMPLES
  
  
}
