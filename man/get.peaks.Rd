\name{get.peaks}
\alias{get.peaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to identify peaks within a dataset.
}
\description{
A subroutine called by functions within the cyphid package. get.peaks is originally from the package msProcess version 1.0.5.  The package was archived on 2012-09-21 and is unvailable as a dependency for cyphid.  The arichived package and original code can be obtained from http://cran.r-project.org/src/contrib/Archive/msProcess/
}
\usage{
get.peaks(x, span = 40)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{span}{
%%     ~~Describe \code{span} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, span = 40) 
  {
    z <- embed(rev(as.vector(x)), dim = span)
    z <- z[rev(seq(nrow(z))), ]
    s <- span\%/\%2
    v <- max.col(z, ties.method = "first") == 1 + s
    z <- c(rep(FALSE, s), v)
    ans <- c(z[1:(length(z) - s)], rep(FALSE, span - 1))
    nx <- NCOL(x)
    if (nx > 1) 
      matrix(ans, ncol = nx)
    else ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
