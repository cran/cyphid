\name{get.cycle.breaks}
\alias{get.cycle.breaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get cycle breakpoints
}
\description{
Internal function to get.all.breaks to get the cycle breakpoints within a chewing sequence.
}
\usage{
get.cycle.breaks(dataset, window, rowLimit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{window}{
%%     ~~Describe \code{window} here~~
}
  \item{rowLimit}{
%%     ~~Describe \code{rowLimit} here~~
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
function(dataset, window, rowLimit=NULL){
	# Get number of observations (i.e, frames) in each sequence
	nobs <- dim(dataset)[1]
	# Get max gape locations in the sequence
	CycleIndex <- msExtrema(dataset,span=window)$index.min
	frames <- seq(1,nobs)
	if(is.null(rowLimit)){
		limits <- NULL
		for(i in 1:dim(dataset)[2]){
		breaks <- frames[CycleIndex[,i]]
		limits <- append(limits, length(breaks))
			}
		rowLimit <- max(limits)
		}
	CycleBreaks <- NULL
	for(i in 1:dim(dataset)[2]){
		breaks <- frames[CycleIndex[,i]]
		CycleBreaks <- add.col(CycleBreaks, breaks, rowLimit)
		}
	return(list(CycleBreaks=CycleBreaks, CycleIndex=CycleIndex))
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
