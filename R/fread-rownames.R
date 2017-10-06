#' @importFrom data.table fread
NULL

##' @title fread for files created with write.csv(..., row.names=TRUE)
##' @param f filename
##' @return fread object, ignoring first column (rownames)
##' @export
##' @author Chris Wallace
freadrn <- function(f) {
    h <- scan(f,nlines=1,what="")
    x <- fread(f,header=FALSE,skip=1)[,-1]
    setnames(x,h)
    x
}
