\name{get.cycle.counts}
\alias{get.cycle.counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get cycle counts
}
\description{
Get the number of cycles within a sequence. Returns a vector with a length equal to the number of sequences evaluated.  Each number in the vector corresponds to the number of cycles within a sequence.
}
\usage{
get.cycle.counts(closeMatrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{closeMatrix}{
%%     ~~Describe \code{closeMatrix} here~~
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
function(closeMatrix){
	cycle.counts <- NULL
	for(i in 1:dim(closeMatrix)[2]){
		ncycles <- length(na.omit(closeMatrix[,i]))
		cycle.counts <- append(cycle.counts, ncycles)
		}
	return(cycle.counts)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
