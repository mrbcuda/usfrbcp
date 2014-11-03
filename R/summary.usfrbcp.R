#' @export
summary.usfrbcp <- function(object,...) {
  writeLines(paste(object[[1]]$Sender$Name,object[[1]]$Name))
  writeLines(paste("Prepared:",object$Header$Prepared))
  writeLines(paste("Data sets:",sum(names(object)=="DataSet")))
  sapply(which(names(object)=="DataSet"),function(i) {
    ds <- object[[i]]
    dsn <- ds$.attrs["id"]["id"]
    writeLines(paste("Data set",dsn,"contains",sum(names(object[[i]])=="Series"),"series"))
  })
  
  invisible()
}