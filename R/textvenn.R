##' Find setdiff()s and intersection of two vectors
##'
##' @title textvenn
##' @param A vector
##' @param B vector
##' @param verbose if TRUE print all items to screen.  Default is FALSE.
##' @param quiet if FALSE (default) report summary counts to screen
##' @return invisibly returns a list containing items unique to A, the
##'     intersection, and items unique to B
##' @export
##' @author Chris Wallace
textvenn <- function(A,B,verbose=FALSE,quiet=FALSE) {
  AnotB <- setdiff(A,B)
  AandB <- intersect(B,A)
  BnotA <- setdiff(B,A)
  if(!quiet) {
    cat("set A:\t",length(AnotB),"\n")
    if(verbose)
        cat(AnotB,"\n")
    cat("intersection:\t",length(AandB),"\n")
    if(verbose)
        cat(AandB,"\n")
    cat("set B:\t",length(BnotA),"\n")
    if(verbose)
        cat(BnotA,"\n")
  }
  invisible(list(A=AnotB,int=AandB,B=BnotA))
}
