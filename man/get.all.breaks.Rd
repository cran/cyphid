\name{get.all.breaks}
\alias{get.all.breaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify all breakpoints. 
}
\description{
This function divides chewing sequnces into individual cycles and identifies the phase transitions within each cycle. 
}
\usage{
get.all.breaks(dataset, CycleBreaks = NULL, window = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
A matrix of chewing sequences.  Each column represents a single sequence.
}
  \item{CycleBreaks}{
A matrix of frame numbers used to force a sequence to break at predetermed locations.  Each column represents the break points for a single sequence.
}
  \item{window}{
The frame limit for which no two cycle breaks can occur.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function returns 8 objects.

\item{openbreaks }{openbreaks, closebreaks, FCSC, and SOFO are matricies with one column for each sequence. Columns contain the frame numbers associated with either the open, close, FCSC, or SOFO loactions for each sequence.}
\item{closebreaks }{see openbreaks}
\item{FCSC }{see openbreaks}
\item{SOFO }{see openbreaks}
\item{cyclemat }{cyclemat is a matrix with one column for each cycle.  Columns contain the displacement values for each cycle.}
\item{close.cycle }{close.cycle, FCSC.cycle, SOFO.cycle is a vector with a value for each cycle. The values represent either the location of close, FCSC, or SOFO relative to the cycle.}
\item{FCSC.cycle }{see close.cycle}
\item{SOFO.cycle }{see close.cycle}

}
%%\references{
%% Reference to paper describing the algorithm.
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
# Run primary function for dividing sequences into cycles and cycles into phases
JawBreaks40 <- get.all.breaks(jaw, window=40)

# Check window based on cycle durations.
cycledurs <- get.cycle.durations(JawBreaks40$cyclemat)
win <- get.window(cycledurs)
win

# Rerun with modified window
JawBreaks27 <- get.all.breaks(jaw, window=27)

# Plot the output
plot(jaw[,1])
abline(v=JawBreaks27$openbreaks[,1])
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
