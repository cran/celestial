\name{deg2hms}
\alias{deg2hms}

\title{
Convert decimal degrees to hms format.
}

\description{
Convert decimal degrees to hms (hours, minutes, seconds) format. This is probably most useful for right-ascension (RA) conversion, since hms is fairly standard method of presenting RA coordinates. The decimal degrees=15*h+15*m/60+15*s/3600 (i.e. there are 24 hours in 360 degrees). Degrees should range from 0 to 360.
}

\usage{
deg2hms(deg)
}

\arguments{
  \item{deg}{
The decimal degrees you are converting. All deg values should be 0<=d<=360.
}
}

\value{
A data.frame with the columns hours, minutes and seconds.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{hms2deg}}
}

\examples{
print(deg2hms(123.456))
}

\keyword{convert}

