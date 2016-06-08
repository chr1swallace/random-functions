##' @title get google sheets as data.table
##' @param url google sheets sharing url
##' @param saveas if not null, save the csv format file at this location.  Must be a valid filename, and will get overwritten!
##' @return data.table
##' @author Chris Wallace
##' @export
getGSheets <- function(url,saveas=NULL) {
    if(is.null(saveas)) {
        f <- tempfile()
    } else {
        f <- saveas
    }
    ss <- strsplit(url,"/")[[1]]
    n <- length(ss)
    if(grepl("sharing",ss[[n]])) {
        ss[[n]] <- "export?format=csv"
    } else {
        stop("url not of recognised format: ",url)
    }
    ## key <- ss[n-1]
    ## url <- paste0("https://docs.google.com/spreadsheets/d/",key,"/export?format=csv")
    url <- paste(ss,collapse="/")
    
    system(paste0("wget --no-check-certificate --output-document=",f," ",url))
    message("head of downloaded file:")
    system(paste("head",f))
    
    require(data.table)
    x <- fread(f)
    if(is.null(saveas))
        unlink(f)
    return(x)
}
