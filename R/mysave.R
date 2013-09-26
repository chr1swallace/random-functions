##' saving into non-existant directories
##'
##' if path points to a non existant directory, creates it before saving as usual
##' @title mysave
##' @param ... passed to save
##' @param file passed to save
##' @return return value of save(..., file=file)
##' @export
##' @author Chris Wallace
mysave <- function(..., file) {
  dir <- dirname(file)
  if(!file.exists(dir))
    dir.create(dir,recursive=TRUE)
  save(..., file=file)
}

