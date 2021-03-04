##' commandArgs parsing
##' 
##' return a named list of command line arguments
##'
##' Usage:
##' call the R script thus
##'   ./myfile.R --args --myarg=something
##' or
##'   R CMD BATCH --args --myarg=something myfile.R
##'
##' Then in R do
##'   myargs <- getArgs()
##' and myargs will be a named list
##'
##' > str(myargs)
##' List of 2
##' $ file : chr "myfile.R"
##' $ myarg: chr "something"
##'
##' To supply a vector as an argument, simply repeat the argument's name:
##'
##' ... --args f=kk f=pp ll
##'
##' > str(myargs)
##' List of 2
##' $ f : Named chr [1:2] "kk" "pp"
##'  ..- attr(*, "names")= chr [1:2] "f" "f"
##' $ ll: logi TRUE
##' 
##' @title getArgs
##' @param defaults a named list of defaults, optional
##' @param verbose print verbage to screen 
##' @param numeric names of arguments that should be converted from character to numeric, optional
##' @return a named list
##' @export
##' @author Chris Wallace
getArgs <- function(defaults=NULL, verbose=FALSE, numeric=NULL) {
  myargs <- gsub("^--","",commandArgs(TRUE))
  setopts <- !grepl("=",myargs)
  if(any(setopts))
    myargs[setopts] <- paste(myargs[setopts],"=notset",sep="")
  myargs.list <- strsplit(myargs,"=")
  myargs <- lapply(myargs.list,"[[",2 )
  names(myargs) <- lapply(myargs.list, "[[", 1)

  ## logicals
  if(any(setopts))
    myargs[setopts] <- TRUE

  ## defaults
  if(!is.null(defaults)) {
    defs.needed <- setdiff(names(defaults), names(myargs))
    if(length(defs.needed)) {
      myargs[ defs.needed ] <- defaults[ defs.needed ]
    }
  }
  
  ## numerics
  if(!is.null(numeric)) {
    numeric <- intersect(numeric, names(myargs))
    if(length(numeric))
      myargs[numeric] <- lapply(myargs[numeric], as.numeric)
  }

  ## merge repeated args
  if(any(duplicated(names(myargs)))) {
      unm <- unique(names(myargs))
      for(nm in unm) {
          wh <- which(names(myargs)==nm)
          if(length(wh)==1)
              next
          myargs[[wh[1]]] <- unlist(myargs[wh])
          myargs[wh[-1]] <- NULL
      }
  }
  
  ## verbage
  ## if(verbose) {
    cat("read",length(myargs),"named args:\n")
    print(myargs)
  ## }
  myargs
}
