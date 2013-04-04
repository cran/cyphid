\name{jaw}
\alias{jaw}
\docType{data}
\title{
Chewing kinematics
}
\description{
This data set provides displacement data during gum chewing. Motion capture was used to collect the kinematic data at a 60 Hz sampling frequency. Two chewing sequences are provided. 
}
\usage{data(jaw)}
\format{
  The format is:
 num [1:1200, 1:2] -0.462 -0.48 -0.506 -0.534 -0.539 -0.537 -0.548 -0.562 -0.574 -0.582 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "vecY" "vecY.1"
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Gerstner, G. E., & Parekh, V. V. (1997). Evidence of Sex-specific Differences in Masticatory Jaw Movement Patterns. Journal of Dental Research, 76(3), 796-806.
}
\examples{
data(jaw)
plot(jaw[,1], xlab="frame number", ylab="displacement")
}
\keyword{datasets}
