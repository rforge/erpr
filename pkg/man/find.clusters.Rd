\name{find.clusters}
\alias{find.clusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 find clusters
}
\description{
 Internal function for \code{mass.t.test}, used in cluster based permutation to find clusters.
}
\usage{
find.clusters(tscores, thresh, chan_hood, thresh_sign, min_nchans)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{tscores}{
a electrode by timepoitns matrix of t-scores
}
  \item{thresh}{
The threshold to be used to filter the t-scores.
}
  \item{chan_hood}{
  an electrode by electrode matrix. As the one created with \link{\code{spatial_neighbours}}
  }
  \item{thresh_sign}{
  a value (-1 or 1), to specify is positive clusters or negative clusters are searcher.
  }
  \item{min_nchans}{
  Minimum number of channels to consider a cluster as valid. 
  }
}
\details{
This is an R translation of find.clusters.m function in MASS univariate toolbox. The \code{min_nchans} has been added by me. The \code{min_nchans}  is inclued as argument to take into account for volume conduction (i.e., it is unlikely that, especially with dense array, an effect is present only in one channel).
}


\value{
A vector with clusters.
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
