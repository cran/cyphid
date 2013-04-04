\name{cyphid-package}
\alias{cyphid-package}
\alias{cyphid}
\docType{package}
\title{
Idenitifies chewing cycles and phases
}
\description{
The primary function in this library automates the process of dividing chewing sequences into cycles and cycles into phases.
}
\details{
\tabular{ll}{
Package: \tab cyphid\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-04-04\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The primary function in this package is get.all.breaks (See example below).
}
\author{

Maintainer: Elizabeth Crane <bcrane@umich.edu>

}
%%\references{

%%}

\keyword{ package }
%%\seealso{

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
