\name{cyphid.function.help}
\alias{add.col}
\alias{BWfilter}
\alias{check.for.sequence.endpoint.breaks}
\alias{create.transition.location.matrix}
\alias{do.transitions.exist}
\alias{get.all.breaks.traditional}
\alias{get.breaks.vector}
\alias{get.cycle.breaks}
\alias{get.cycle.counts}
\alias{get.cycle.durations}
\alias{get.cycle.matrix}
\alias{get.cycle.minimums}
\alias{get.trans.FDiff}
\alias{get.trans}
\alias{get.transition.locations.FDiff}
\alias{get.transition.locations}
\alias{get.window.estimate}
\alias{get.window}
\alias{is.dividing.valid}
\alias{rm.end.NAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subroutine
%Add column to matrix
}
\description{
This function is a subroutine called by the funtion get.all.breaks in the cyphid package.
%Add a single column to a matrix when the number of observations varies between columns.  
}
%\usage{
%add.col(df, new.col, MaxTime)
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{df}{
%Matrix
%}
%  \item{new.col}{
%Data that is to be added to matrix
%}
%  \item{MaxTime}{
%Predetermined number of rows the matrix should have.  This should equal the record with the largest number of observations.
%}
%}
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
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function(df, new.col, MaxTime) {n.row<-dim(df)[1]
%	if(is.null(n.row)){n.row<-MaxTime}
%           length(new.col)<-n.row
%           cbind(df, new.col)
%    }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
