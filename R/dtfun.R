##' dcast a data.table, and return a matrix
##'
##' @title data.table to matrix
##' @param dt data.table
##' @param ... args passed to data.table::dcast
##' @return a matrix
##' @export
##' @author Chris Wallace
dt2mat <- function(dt,...) {
    tmp <- data.table::dcast(dt,...)
    rn <- tmp[[1]]
    m <- as.matrix(tmp[,-1])
    rownames(m) <- rn
    m
}
