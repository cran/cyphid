\name{get.window}
\alias{get.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get window limit
}
\description{
Function to identify the window limit for identifying cycle endpoints within a seqeunce. This tool is used to determine a reasonable number of frames that must separate two cycle endpoints.
}
\usage{
get.window(dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
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
function(dataset){
	xbar <- round(mean(dataset),1)
	xsd <- round(sd(dataset),1)
	upper <- xbar + (2*xsd)
	lower <- xbar - (2*xsd)
	win <- round(upper - lower,0)
	return(win)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
