##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title nicify cowplot theme
##' @param p plot object
##' @return nicer plot object
##' @examples
##' library(ggplot2)
##' library(cowplot)
##' df <- data.frame(x=rnorm(1000),y=rnorm(1000))
##' p <- ggplot(df, aes(x=x,y=y)) + geom_point() + geom_density2d() + ggtitle("2D data")
##' p
##' nicecow(p)
##' @author Chris Wallace
##' @export
nicecow <- function(p) p + theme(plot.title=element_text(hjust=0)) + background_grid()

