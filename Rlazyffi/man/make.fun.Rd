\name{make.fun}
\alias{make.fun}
\title{
make a C function wrapper
}
\description{
  make a C function wrapper
}
\usage{
make.fun(symbol)
}
\arguments{
  \item{symbol}{
    C function name
  }
}
\value{
  The newly built R function
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
f <- make.fun("printf")
}
\keyword{ programming }
