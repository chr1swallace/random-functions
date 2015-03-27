##' Check whether files exist (or don't exist), and stop()
##'
##' I often want to run a script only if a particular file or files
##' already exist.  Or perhaps only run a script if the output file
##' doesn't already exist.  To avoid writing a lot of
##' =if(file.exists(f)) {}=
##' this is a convenience function.
##' @title check whether files exist
##' @param ... files to check
##' @param invert check for existence of a file, instead of non-existence
##' @return no return value
##' @export
##' @examples
##' ## Should do nothing
##' checkfile(system.file(c("INDEX","help")))
##' checkfile(system.file(c("ThisFileShouldNotExist")),invert=TRUE)
##' ## Should call stop()
##' checkfile(system.file(c("INDEX","help")),invert=TRUE)
##' checkfile(system.file(c("ThisFileShouldNotExist")))
##' @author Chris Wallace
checkfile <- function(...,invert=FALSE) {
  files <- c(...)
  ex <- file.exists(files)
  for(i in seq_along(ex))
    if(!xor(ex[i],invert))
      stop("File ",ifelse(invert,"","not "),"found: ",files[i])
}
