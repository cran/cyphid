\name{get.breaks.vector}
\alias{get.breaks.vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create breakpoint vector
}
\description{
Internal function to get breakspoints and put them into a vector.
}
\usage{
get.breaks.vector(CycleBreaksMatrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CycleBreaksMatrix}{
Each column in the matrix contains the cycle breakpoints within a sequence.
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
function(CycleBreaksMatrix){
	breaks.vector <- NULL
	for(i in 1:dim(CycleBreaksMatrix)[2]){
		set <- na.omit(CycleBreaksMatrix[,i])
		lset <- length(set)-1
		breaks.vector <- append(breaks.vector, set[1:lset])
		}	
	return(breaks.vector)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
