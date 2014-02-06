\name{vlist.bind}
\alias{vlist.bind}
\title{vlist.bind}
\usage{
vlist.bind(l, direction = "c")
}
\arguments{
  \item{l}{a list of vectors}

  \item{direction}{"c" for column binding or "r" for
  rowbinding}
}
\value{
a matrix
}
\description{
Bind a list of vectors of equal length together
}
\details{
Useful for very large lists, when \code{do.call("rbind",l)}
would be unacceptably slow.
}
\author{
Chris Wallace
}
\seealso{
\link{mlist.bind} for binding lists of matrices
}

