#' @export
summary.usfrbcpo <- function(object,...) {
  writeLines(paste("Prepared:",object$prepared))
  sapply(2:length(object),function(i) {
    writeLines(paste(object[[i]]$series_name,object[[i]]$short_desc))
  })
  invisible()
}
