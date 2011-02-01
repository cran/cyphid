\name{get.trans}
\alias{get.trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get transitions
}
\description{
Identify transitions locations within each cycle.
}
\usage{
get.trans(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Individual cycle data.
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
function(dat){
		lambda <- .9 
		norder <- 6
		samples <- seq(1,length(dat), length=length(dat))
		nbasis <- length(samples) + norder-2
		basis <- create.bspline.basis(c(1,length(dat)), nbasis, norder, samples)
		fdPar <- fdPar(basis, 4, lambda)
		fd <- smooth.basis(samples, dat, fdPar)$fd
		accfd <- deriv.fd(fd,2)
		acc <- eval.fd(samples, accfd)
		frames <- seq(1,length(dat))
		minindex <- msExtrema(acc)$index.min
		minframes <- frames[minindex]
		mindat <- acc[minindex]
		transmin <- which.min(mindat)
		return <- list(transmin=transmin, minframes=minframes, acc=acc)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
