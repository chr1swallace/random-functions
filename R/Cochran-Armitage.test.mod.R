##' Cochran Armitage test - with Mantel extension
##'
##' See Clayton & Hills 20.3 pp 201-203 for statistical details
##' 
##' This version is modified (bugfixed) from an original function by
##' David Clayton as below
##'
##' change
##' x2 <- sum(ut^2/vt,na.rm=T)
##' to
##' x2 <- sum(ut,na.rm=T)^2/sum(vt,na.rm=T)
##' 
##' Modified Feb 2014 to change
##' ut <- dh[,1]*dh[,2]*(zm[,1] - zm[,2])/nt
##' to
##' ut <- dh[,1]*dh[,2]*(zm[,2] - zm[,1])/nt
##' so that sign(ut)>0 for positive correlation
##' @param exposure vector
##' @param cc vector
##' @param stratum (optional) vector of strata
##' @export
##' @author David Clayton, with modifications by Chris Wallace
Cochran.Armitage.test <- function(exposure, cc, stratum=rep(1,length(cc))) {
  N <- length(cc)
  if (is.factor(exposure))
    exposure <- as.numeric(exposure)
  cl <- match.call()
  narg <- length(cl) - 1
  arguments <- character(narg-1)
#   for (i in 1:narg)
#     arguments[i] <- as.character(cl[[i+1]])
  use <- !(is.na(cc) | is.na(exposure) | is.na(stratum))
  if (any(!use)) {
    cc <- cc[use]
    exposure <- exposure[use]
    stratum <- stratum[use]
  }
  dh <- table(stratum, cc)
  if (ncol(dh)!=2)
    stop("cc argument must have two levels")
  nt <- table(stratum)
  zm <- tapply(exposure, list(stratum, cc), mean)
  ut <- dh[,1]*dh[,2]*(zm[,2] - zm[,1])/nt
  zv <- tapply(exposure, stratum, var)
  vt <-  dh[,1]*dh[,2]*zv/nt
  x2 <- sum(ut,na.rm=T)^2/sum(vt,na.rm=T)
  names(x2) <- "Chi-squared"
  df <- 1
  names(df) <- "df"
  res <- list(statistic=x2, parameter=df,
              p.value=pchisq(x2, 1, lower.tail=FALSE),
              method="Cochran-Armitage test with Mantel's extension",
              data.name=paste(cl,collapse=" "),
              score=ut,
              score.variance=vt)
  class(res) <- "htest"
  res
}

