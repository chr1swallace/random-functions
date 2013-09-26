##' Bind a list of vectors of equal length together 
##'
##' Useful for very large lists, when \code{do.call("rbind",l)} would be unacceptably slow.
##' @title vlist.bind
##' @param l a list of vectors
##' @param direction "c" for column binding or "r" for rowbinding
##' @return a matrix
##' @export
##' @author Chris Wallace
##' @seealso \link{mlist.bind} for binding lists of matrices
vlist.bind <- function(l,direction="c") {
  ## bind a list of vectors together
  n <- length(l)
  m <- length(l[[1]])
  lengths <- sapply(l,length)
  if(!all(lengths==m))
    stop("all elements need to be of equal length")
  if(direction=="c") { # cbind
    ret <- matrix(NA,m,n,dimnames=list(names(l[[1]]), names(l)))
    for(i in 1:n)
      ret[,i] <- l[[i]]
  } else {
    ret <- matrix(NA,n,m,dimnames=list(names(l), names(l[[1]])))
    for(i in 1:n)
      ret[i,] <- l[[i]]
  }
  return(ret)
}
##' Bind a list of vectors of equal row or col dim together 
##'
##' Useful for very large lists, when \code{do.call("rbind",l)} would be unacceptably slow.
##' @title mlist.bind
##' @param l a list of matrices
##' @param direction  "c" for column binding or "r" for rowbinding
##' @return a matrix
##' @export
##' @author Chris Wallace
##' @seealso \link{vlist.bind} for binding lists of vectors
mlist.bind <- function(l,direction="c") {
  ## bind a list of matrices together
  N <- length(l)
  m <- sapply(l,nrow)
  n <- sapply(l,ncol)
  if(direction=="c" & !all(m==m[[1]]))
    stop("all elements need to have equal nrow")
  if(direction!="c" & !all(n==n[[1]]))
    stop("all elements need to have equal ncol")
  if(direction=="c") { # cbind
    if(is(l[[1]],"SnpMatrix")) {
      ret <- new("SnpMatrix", matrix(as.raw(0),m[1],sum(n),dimnames=list(rownames(l[[1]]), unlist(sapply(l,colnames)))))
    } else {
      ret <- matrix(0,m[1],sum(n),dimnames=list(rownames(l[[1]]), unlist(sapply(l,colnames))))
    }
    n <- c(0,cumsum(n))
    for(i in 1:N) {
      cat(".")
      idx <- (n[i]+1):(n[(i+1)])
      ret[,idx] <- l[[i]]
    }
  } else { # rbind
    if(is(l[[1]],"SnpMatrix")) {
      ret <- new("SnpMatrix", matrix(as.raw(0),sum(m),n[1],dimnames=list(rownames(l[[1]]), unlist(sapply(l,colnames)))))
    } else {
      ret <- matrix(0,sum(m),n[1],dimnames=list(rownames(l[[1]]), unlist(sapply(l,colnames))))
    }
    m <- c(0,cumsum(m))
    for(i in 1:N) {
      cat(".")
      idx <- (n[i]+1):(n[(i+1)])
      ret[idx,] <- l[[i]]
    }
  }
  return(ret)
}
