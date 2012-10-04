\name{deg2dms}
\alias{deg2dms}

\title{
Convert decimal degrees to dms format.
}

\description{
Convert decimal degrees to dms (degrees, minutes, seconds) format. This is probably most useful for declination conversion, since dms is fairly standard method of presenting declination coordinates. The decimal degrees=d+m/60+s/3600. Degrees should range from -90 to +90.
}

\usage{
deg2dms(deg)
}

\arguments{
  \item{deg}{
The decimal degrees you are converting. All deg values should be -90<=deg<=90
}
}

\value{
A data.frame with the columns degrees, minutes and seconds.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{dms2deg}}
}

\examples{
print(deg2dms(12.345))
}

\keyword{convert}

