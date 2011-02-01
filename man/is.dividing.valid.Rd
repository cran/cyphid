\name{is.dividing.valid}
\alias{is.dividing.valid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Is dividing valid
}
\description{
Internal function to get.all.breaks that determines whether the FCSC or the SOFO transitions are valid.
}
\usage{
is.dividing.valid(cycledat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cycledat}{
%%     ~~Describe \code{cycledat} here~~
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
function(cycledat){
			if(length(cycledat) > 3){valid = TRUE}else{valid <- FALSE}
			return(valid)
			}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
