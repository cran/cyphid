\name{get.cycle.matrix}
\alias{get.cycle.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build a matrix of cycle data
}
\description{
This function create a matrix of cycle data.  Each column contains the kinematic data associated with a unique chewing cycle.
}
\usage{
get.cycle.matrix(dataset, CycleBreaks, rowLimit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
Raw chewing sequences.
}
  \item{CycleBreaks}{
Cycle breakpoints for each sequence.
}
  \item{rowLimit}{
If known, the maximum number of rows required for the new matrix. This is important since cycles vary in duration.
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
function(dataset, CycleBreaks, rowLimit=NULL){
	limits <- NULL
	if(is.null(rowLimit)){
		for(i in 1:dim(CycleBreaks)[2]){
			breaks <- na.omit(CycleBreaks[,i])
			for(a in 1:(length(breaks)-1)){
				start <- breaks[a]
				end <- breaks[a+1]
				cycle <- dataset[start:end,i]
				limits <- append(limits, length(cycle))
				}
			}
		rowLimit <- max(limits)
		}
	cyclemat <- NULL	
	for(i in 1:dim(CycleBreaks)[2]){
		breaks <- na.omit(CycleBreaks[,i])	
		for(a in 1:(length(breaks)-1)){
			start <- breaks[a]
			end <- breaks[a+1]
			cycle <- dataset[start:end,i]
			cyclemat <- add.col(cyclemat, cycle, MaxTime=rowLimit) 
			}
		}
	return(cyclemat)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
