\name{dyn.constant}
\alias{dyn.constant}
\title{
  wrap a set of C constants into R function with the same name
}
\description{
  wrap a set of C constants into R function with the same name
}
\usage{
dyn.constant(symbs, envir=parent.frame(), type=pointerType)
}
\arguments{
  \item{symbs}{
    vector of C constants names
  }
  \item{envir}{
    environment in which to put the newly created R functions
  }
  \item{envir}{
    C type of the constants
  }
}
\value{
  Nothing useful
}
\author{
  Antonio, Fabio Di Narzo
}
\examples{
dyn.constant(c("stdout", "stderr"))
stdout()
stderr()
}
\keyword{ programming }
