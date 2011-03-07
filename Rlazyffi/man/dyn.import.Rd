\name{dyn.import}
\alias{dyn.import}
\title{
  wrap a set of C function into R function with the same name
}
\description{
  wrap a set of C function into R function with the same name
}
\usage{
dyn.import(symbs, envir = parent.frame())
}
\arguments{
  \item{symbs}{
    vector of C function names
  }
  \item{envir}{
    environment in which to put the newly created R functions
  }
}
\value{
  Nothing useful
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
dyn.import("printf")
printf("hello %s!\n", "world")
printf("some more format strings: %d, %.3f\n", 10L, 10.0)
}
\keyword{ programming }
