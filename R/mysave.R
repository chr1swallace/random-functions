##' run code or load previous run
##'
##' load a file if it exists. Otherwise, run commands, and save created objects.
##' Experimental!
##' @param file string, name of file to save in
##' @param objects character vector giving names of objects to save
##' @param ... arguments to be evaluated. Use "<-" not "=" for any objects to be
##'   saved!
##' @export
##' @return side effect: loads from file
##' @examples
##' runorload("tmp.RData","xnew",ls(),a=2,b=6,xnew<-a*b)
##' @author Chris Wallace
runorload <- function(file, objects, ...) {
  if(!file.exists(file)) {
    message("file not found: ",file)
    message("running...")
    ## exprs <- #c(as.list(
      exprs <- match.call(expand.dots = FALSE)$...
    ## ,list)
    lapply(exprs, function(e) eval(e, env=.GlobalEnv))
    save(list=objects, file=file)
  } else {
    message("loading from ",file)
    obj=load(file, .GlobalEnv)
    if(!all(objects %in% obj)) {
      warning("not all listed objects found in ",file,
              "\nexpected: ",paste(objects,collapse=" "),
              "\nfound: ",paste(obj,collapse=" "))
    }
  }
}

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

