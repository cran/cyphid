\name{get.transition.locations}
\alias{get.transition.locations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get transition locations
}
\description{
Report transition locations relative to the cycle as well as relative to the sequence.
}
\usage{
get.transition.locations(cyclemat, close.cycle, type, CycleBreaks = NULL, closebreaks, frameLimit = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cyclemat}{
cycle data.
}
  \item{close.cycle}{
Maximum close for each cycle.
}
  \item{type}{
FCSC or SOFO.
}
  \item{CycleBreaks}{
Cycle breakpoints within the sequence.
}
  \item{closebreaks}{
Maximum cycle close locations within a sequence.
}
  \item{frameLimit}{
  Minimum number of frames required to consider a phase as valid (i.e. actually occuring).
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
function(cyclemat, close.cycle, type, CycleBreaks = NULL, closebreaks, frameLimit = NULL){
	if(is.null(frameLimit)){frameLimit <- 3}
	cycle <- NULL
	sequence <- NULL
	for(i in 1:dim(cyclemat)[2]){
		if(type=="FCSC"){
			dat <- cyclemat[1:close.cycle[i],i]
			}
		if(type=="SOFO"){
			cycleLength <- length(na.omit(cyclemat[,i]))
			dat <- cyclemat[close.cycle[i]:cycleLength,i]
			}
		if(is.dividing.valid(dat) == TRUE){
			out <- get.trans(dat)
			if(do.transitions.exist(out$transmin) == FALSE){
				cycle <- append(cycle, NA)
				}
			if(do.transitions.exist(out$transmin) == TRUE){
				nframes.btwn.close.and.transition <- abs(length(dat) - out$minframes[out$transmin])
				if(nframes.btwn.close.and.transition > frameLimit){
						if(type=="FCSC"){
							cycle <- append(cycle, out$minframes[out$transmin])
							}
						if(type=="SOFO"){
							cycle <- append(cycle, (out$minframes[out$transmin] + close.cycle[i])-1)
							}
						}else{
							cycle <- append(cycle, NA)
							}	
					}
			}
		if(is.dividing.valid(dat) == FALSE){
			cycle <- append(cycle, NA)
			}
		}
	
	if(!is.null(CycleBreaks)){
		breaks.vector <- get.breaks.vector(CycleBreaks)
		sequence <- cycle + breaks.vector
		cycleCounts <- get.cycle.counts(closebreaks)
		mat <- create.transition.location.matrix(sequence, cycleCounts)
		}
	return(list(cycle=cycle, sequence=sequence, seq.mat=mat))
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
