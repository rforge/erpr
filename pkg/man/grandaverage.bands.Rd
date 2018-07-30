\name{grandaverage.bands}
\alias{grandaverage.bands}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate value to plot the bands for an ERP plot.
}
\description{
This function can be used to calculate the values to plot error bands around an ERP. It calculates the bands fro the output of \code{grandaverage.se}, and of \code{grandaverage}.

}
\usage{
grandaverage.bands(grandaverage = NULL, grandaverage.se = NULL, electrode = NULL)
}

\arguments{
  \item{grandaverage}{
The object containing a grandaverage of ERPs (typically the output of \code{grandaverage} function of \code{erpR})
}
  \item{grandaverage.se}{
The object containing the standard error of ERPs (typically the output of \code{grandaverage.se} function of \code{erpR})
}

\item{electrode}{
a character vector indicating the electrode on which calculate the band. 
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A data frame with two columns: \code{upper} with the upper band, and \code{lower} with the lower band.
}

\author{
Giorgio Arcara
}

%\note{
%The function keeps track of the averaged objects in a \code{comment}.
%}

\seealso{
\code{\link{grandaverage}},\code{\link{grandaverage.se}}
}


\examples{

\dontrun{

######################################################
# LOAD AND PREPARE DATA FOR PLOTTING  ################
#####################################################
# load data
data(erplistExample)

# calculate grandaverage
word=grandaverage("Exp1_word_subj", 1:20, erplist=erplistExample)

# calcualte se
word.se = grandaverage.se ("Exp1_word_subj", 1:20, erplist=erplistExample)

# calculate bands on a given electrode
my_el = "FPZ"
myband = grandaverage.bands(word, word.se, my_el)


######################################################
# PLOT WITH ERP BANDS (ONLY ERPR FUNCTIONS AND BASE R)
#####################################################

# plot electrode
erp(word, my_el, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-6,6))
# add band
polygon(x=c(1:length(word[,my_el]), length(word[, my_el]):1), 
  y = c(myband$lower, rev(myband$upper)), col=rgb(0.8, 0.8, 0.8,0.5))
# NOTE! I re-plot the electrode over.
erp.add(word, my_el, lwd=2) 

######################################################
# PLOT WITH ERP BANDS (WITH GGPLOT2)
#####################################################

require(ggplot2)


word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
time = pointstomsec(1:dim(word)[[1]], startmsec=-500, 
  endmsec=1500, lengthsegment = dim(word)[[1]])

### WITH GGPLOT

ggplot(word, aes(time, FPZ))+
  geom_line(size=1)+
  #theme_bw()+
  geom_ribbon(data = myband, aes(ymin=lower,ymax=upper), alpha=0.3, col="darkgray") +
  theme(panel.grid.major=element_line(color="lightgray"), 
        panel.background = element_blank())

}




}

