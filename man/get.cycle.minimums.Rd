\name{get.cycle.minimums}
\alias{get.cycle.minimums}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get cycle minimums
}
\description{
Get maximum close for each cycle in the dataset.
}
\usage{
get.cycle.minimums(dataset, CycleBreaks, cyclemat, rowLimit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{CycleBreaks}{
%%     ~~Describe \code{CycleBreaks} here~~
}
  \item{cyclemat}{
%%     ~~Describe \code{cyclemat} here~~
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
function(dataset, CycleBreaks, cyclemat, rowLimit=NULL){
	# Get cycle minimums with respect to the cycle
	close.cycle <- NULL
	for(i in 1:dim(cyclemat)[2]){
		close.cycle <- append(close.cycle, which.max(cyclemat[,i]))
		}
	# Get cycle minimums with respect to the cycle sequence
	if(is.null(rowLimit)){
		limits <- NULL
		for(i in 1:dim(CycleBreaks)[2]){
			set1 <- na.omit(CycleBreaks[,i])
			ncycles <- length(set1)-1
			limits <- append(limits, ncycles)
			}
		rowLimit <- max(limits)
		}
	closebreaks <- NULL
	start <- 1
	for(i in 1:dim(CycleBreaks)[2]){
		set1 <- na.omit(CycleBreaks[,i])
		nbreaks <- length(set1)
		ncycles <- nbreaks-1
		open <- CycleBreaks[1:ncycles, i]
		end <- start + ncycles - 1
		close <- close.cycle[start:end]
		set2 <- open + close
		closebreaks <- add.col(closebreaks, set2, rowLimit)
		start <- end + 1
		}
	return(list(closebreaks=closebreaks, close.cycle=close.cycle))
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
