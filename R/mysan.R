##' Sanitizer function for xtable that converts scientific notation to latex style
##'
##' @param x character or numeric vector 
##' @return sanitized character
##' @export
##' @examples
##' p<-10^(-seq(1,10))
##' format.pval(p) # scientific notation
##' mysan(p) # latex style
##'
##' library(xtable)
##' df <- data.frame(numeric=p,scientific=format.pval(p),p.san=mysan(p))
##' print(xtable(df)) # doesn't work
##' print(xtable(df), sanitize.text.function=mysan)  # looks nice!
mysan <- function(x) {
  if(is.numeric(x))
    x <- format.pval(x)
  sub("(<? ?[0-9\\.]+)e-([0-9]+)","$\\1\\\\times10^{-\\2}$",x)
}
