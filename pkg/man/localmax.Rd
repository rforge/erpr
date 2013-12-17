\name{localmax}
\alias{localmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
localmax
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
localmax function(x, n=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

function(x, n=2){
	vet=x
	dat=data.frame(index=1:length(vet))
	# creo un data.frame con gli indici. Mi serve per utilizzare la funzione apply.
	
	test=function(index,vet,n) {
	indices=c((index-n):(index-1), (index+1):(index+n))
	indices=indices[indices>0&indices<(length(vet))]
	response=((!any(vet[indices]>vet[index]))&length(indices)==(n*2))
	return(response)
	}
	
	candidates.indices=apply(dat, 1, function(x){test(index=x, vet, n)})
	
	if(any(candidates.indices)){
	candidates=vet[candidates.indices]
	return(max(candidates)) } else {
		return(NA)
	}
	
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
