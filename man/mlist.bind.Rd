% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binding.R
\name{mlist.bind}
\alias{mlist.bind}
\title{mlist.bind}
\usage{
mlist.bind(l, direction = "c")
}
\arguments{
\item{l}{a list of matrices}

\item{direction}{"c" for column binding or "r" for rowbinding}
}
\value{
a matrix
}
\description{
Bind a list of matrices of equal row or col dim together
}
\details{
Useful for very large lists, when \code{do.call("rbind",l)} would be unacceptably slow.
}
\seealso{
\link{vlist.bind} for binding lists of vectors
}
\author{
Chris Wallace
}
