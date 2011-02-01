\name{create.transition.location.matrix}
\alias{create.transition.location.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a matrix of transition locations
}
\description{
Internal function to get.all.breaks.
}
\usage{
create.transition.location.matrix(transitionVector, cycleCounts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{transitionVector}{
%%     ~~Describe \code{transitionVector} here~~
}
  \item{cycleCounts}{
%%     ~~Describe \code{cycleCounts} here~~
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
function(transitionVector, cycleCounts){
	MaxTime <- max(cycleCounts)
	end <- 0
	transMat <- NULL
	for(i in 1:length(cycleCounts)){
		start <- end + 1
		end <- start + cycleCounts[i] - 1
		set1 <- transitionVector[start:end]
		transMat <- add.col(transMat, set1, MaxTime)
		}
	return(transMat)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
