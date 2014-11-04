#' @title Get commercial paper history.
#' @description Fetches the US Federal Reserve Bank's commercial paper data, parses and returns as list.
#' @details Downloads from the Federal Reserve Bank's web service a zip file of several XML and XSD files 
#'   related to commercial paper rates, volumes, and outstanding quantities over time.  Extracts the
#'   commercial paper XML files in a temporary directory and parses its XML into a list. The web service
#'   URL used is \url{http://www.federalreserve.gov/datadownload/Output.aspx?rel=CP&filetype=zip}.
#' @author Matt Barry <mrb@@softisms.com>
#' @export
#' @importFrom XML xmlParse xmlToList
#' @importFrom utils unzip download.file
#' @return Returns the many data series as a list of class \code{usfrbcp}.  
#' @seealso \code{\link{getCommercialPaperRates}} for extracting commercial paper rates, 
#' \code{\link{getCommercialPaperRatesOld}} for extracting old commercial paper rates, 
#' \code{\link{getCommercialPaperVolumes}} for extracting commercial paper volumes, 
#' \code{\link{getCommercialPaperOutstanding}} for extracting commercial paper outstanding, 
#' \code{\link{getCommercialPaperOutstandingOld}} for extracting old commercial paper outstanding, 
#' \code{\link{getCommercialPaperYearend}} for extracting commercial paper outstanding at year end, 
#' \code{\link{mergeSeries}} for merging list elements into a data frame.
#' @family aggregate functions
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' summary(cp)
#' }
getCommercialPaper <- function() {
  
  resource <- "http://www.federalreserve.gov/datadownload/Output.aspx?rel=CP&filetype=zip"
  td <- tempdir()
  tf <- tempfile(tmpdir=td,fileext=".zip")
  utils::download.file(resource,tf)
  fname <- "CP_data.xml"
  utils::unzip(tf,files=fname,overwrite=TRUE,exdir=td)
  fpath=file.path(td,fname)
  doc <- XML::xmlParse(fpath)
  unlink(tf)
  if (is.null(doc)) {
    warning(paste("Could not parse the location",fpath))
    return(NULL)
  }
  
  message("Download and parse complete.  Converting to list...")
  
  rv <- XML::xmlToList(doc)
  message("List conversion complete.")
  unlink(fpath)
  
  class(rv) <- "usfrbcp"
  return(rv)
}

#' @title Merge commercial paper series.
#' @description Combines the commercial paper series lists into a data frame with series as columns 
#' and dates as row names.
#' @param data an extracted commercial paper series from rates, volume, outstanding, etc.
#' @return If \code{data} has class \code{usfrbcpo}, which contains both monthly and 
#'   weekly series, then the return value is a list of two data frames 
#'   \code{weekly} and \code{monthly}; otherwise, the return value is a data frame.
#' @export 
#' @seealso \code{\link{getCommercialPaperRates}} for extracting commercial paper rates, 
#' \code{\link{getCommercialPaperRatesOld}} for extracting old commercial paper rates, 
#' \code{\link{getCommercialPaperVolumes}} for extracting commercial paper volumes, 
#' \code{\link{getCommercialPaperOutstanding}} for extracting commercial paper outstanding, 
#' \code{\link{getCommercialPaperOutstandingOld}} for extracting old commercial paper outstanding, 
#' \code{\link{getCommercialPaperYearend}} for extracting commercial paper outstanding at year end.
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' rates <- getCommercialPaperRates(cp,"text")
#' rt <- mergeSeries(rates)
#' tail(rt)
#' }
mergeSeries <- function(data) {
  if ( class(data) == "usfrbcpo" ) {
    names = do.call(cbind,lapply(2:length(data),function(i) data[[i]]$series_name))
    weekly = which(grepl("\\.WW",names))
    monthly = seq(weekly[length(weekly)]+1,length(names))
    table.w = do.call(cbind,lapply(weekly+1,function(i) data[[i]]$df))
    table.m = do.call(cbind,lapply(monthly+1,function(i) data[[i]]$df))
    colnames(table.w) <- names[weekly]
    colnames(table.m) <- names[monthly]
    table=list(weekly=table.w,monthly=table.m)
  } else {
    table = do.call(cbind,lapply(2:length(data),function(i) data[[i]]$df))
    names = do.call(cbind,lapply(2:length(data),function(i) data[[i]]$series_name))
    colnames(table) <- names
  }
  table  
}


#' @title Extract commercial paper series.
#' @details Extracts a given series from the commercial paper data sets. 
#' Not meant to be used directly, but useful if class name assignment is unwanted.  
#' Substitutes observation value \code{NA} for any off-nominal observation status indicators.
#' @param x commercial paper data sets as from \code{\link{getCommercialPaper}}.
#' @param seriesIndex the series number from within the data set.
#' @param columnName name for the extracted observation value column.
#' @param progressBar optional progress bar for \code{llply} progress monitoring, default none; see \code{plyr::create_progress_bar}.
#' @return Returns a list containing prepared date and series data frame elements.
#' @importFrom plyr llply ldply
#' @seealso \code{\link{getCommercialPaper}}, \code{\link{mergeSeries}}
extractCommercialPaperSeries <- function(x,seriesIndex,columnName,progressBar="none") {
  prepared <- x$Header$Prepared # extract prepared date
  series <- x[[seriesIndex]] # extract the data series 
  series[[length(series)]] <- NULL # trim the attributes element
  
  # create a list of items by series
  # each list element is a list of series, annotation, and data frame
  y <- llply(series,.fun=function(e) {
    
    # extract series name from last element
    currency <- e[[length(e)]]["CURRENCY"]
    cp_mat_range <- e[[length(e)]]["CP_MAT_RANGE"]
    cp_type <- e[[length(e)]]["CP_TYPE"]
    cp_vol_type <- e[[length(e)]]["CP_VOL_TYPE"]
    cp_freq <- e[[length(e)]]["FREQ"]
    cp_unit <- e[[length(e)]]["UNIT"]
    cp_unit_mult <- e[[length(e)]]["UNIT_MULT"]
    series_name <- e[[length(e)]]["SERIES_NAME"]
    e[[length(e)]] <- NULL
    
    # extract annotation text from first element
    short_description <- e[[1]][1][1]$Annotation$AnnotationText
    names(short_description) <- "SHORT.DESC"
    long_description <- e[[1]][2][1]$Annotation$AnnotationText
    names(long_description) <- "LONG.DESC"
    e[[1]] <- NULL
    
    # the remaining elements are observations
    # we've removed first and last elements above
    df <- ldply(e,function(f) {
      if ( f["OBS_STATUS"] != "A")
        f["OBS_VALUE"] <- NA
      c(f["TIME_PERIOD"],f["OBS_VALUE"])
    },.id="TIME_PERIOD")
    
    # move time period to row names, then remove column
    dates <- df$TIME_PERIOD
    df <- data.frame(as.double(df[,-1]))
    rownames(df) <- dates
    colnames(df) <- columnName
    
    list(series_name=series_name,
         currency=currency,
         mat_range=cp_mat_range,
         type=cp_type,
         vol_type=cp_vol_type,
         freq=cp_freq,
         unit=cp_unit,
         unit_mult=cp_unit_mult,
         short_desc=short_description,
         long_desc=long_description,
         df=df)
  },.progress=progressBar)
  
  # prepend the report-prepared date to the list elements
  rv <- c(list(prepared=prepared),y)
}


#' @title Extract commercial paper rates data.
#' @description Extracts the commercial paper rates series from the commercial paper data set.
#' @param cp commercial paper data as retrieved by \code{getCommercialPaper()}.
#' @param progressBar optional progress bar for \code{llply} progress monitoring, 
#' default none; see {\code{plyr::create_progress_bar}}.
#' @return Returns a list containing a data frame for each series, class \code{usfrbcpr}.
#' @export
#' @seealso \code{\link{getCommercialPaper}}, 
#' \code{\link{getCommercialPaperRatesOld}}, 
#' \code{\link{getCommercialPaperVolumes}}, 
#' \code{\link{getCommercialPaperOutstanding}}, 
#' \code{\link{getCommercialPaperOutstandingOld}}, 
#' \code{\link{getCommercialPaperYearend}}, 
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' rates <- getCommercialPaperRates(cp,"text")
#' summary(rates)
#' }
getCommercialPaperRates <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=2,
                                     columnName="Rate",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpr"
  return(rv)
}

#' @title Extract the commercial paper volumes data.
#' @description Extracts the commercial paper volume series from the commercial paper data set.
#' @inheritParams getCommercialPaperRates
#' @return Returns a list containing data frame for each series, class \code{usfrbcpv}
#' @export
#' @seealso \code{\link{getCommercialPaper}}, 
#' \code{\link{getCommercialPaperRates}}, 
#' \code{\link{getCommercialPaperRatesOld}}, 
#' \code{\link{getCommercialPaperYearend}}, 
#' \code{\link{getCommercialPaperOutstanding}}, 
#' \code{\link{getCommercialPaperOutstandingOld}}, 
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' volumes <- getCommercialPaperVolumes(cp,"text")
#' summary(volumes)
#' }
getCommercialPaperVolumes <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=3,
                                     columnName="Rate",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpv"
  return(rv)
}

#' @title Extract the commercial paper outstanding data.
#' @description Extracts the commercial paper outstanding series from the commercial paper data set.
#' @inheritParams getCommercialPaperRates
#' @return list containing data frame for each series, class \code{usfrbcpo}.
#' @export
#' @seealso \code{\link{getCommercialPaper}}, 
#' \code{\link{getCommercialPaperRates}}, 
#' \code{\link{getCommercialPaperRatesOld}}, 
#' \code{\link{getCommercialPaperVolumes}}, 
#' \code{\link{getCommercialPaperOutstandingOld}}, 
#' \code{\link{getCommercialPaperYearend}}, 
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' outstanding <- getCommercialPaperOutstanding(cp,"text")
#' summary(outstanding)
#' }
getCommercialPaperOutstanding <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=4,
                                     columnName="Outstanding",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpo"
  return(rv)
}

#' @title Extract the commercial paper outstanding year end data.
#' @description Extracts the commercial paper year end series from the commercial paper data set.
#' @inheritParams getCommercialPaperRates
#' @return Returns a list containing data frame for each series, class \code{usfrbcpy}.
#' @export
#' @seealso \code{\link{getCommercialPaper}}, 
#' \code{\link{getCommercialPaperRates}}, 
#' \code{\link{getCommercialPaperRatesOld}}, 
#' \code{\link{getCommercialPaperVolumes}}, 
#' \code{\link{getCommercialPaperOutstanding}}, 
#' \code{\link{getCommercialPaperOutstandingOld}}, 
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' yearend <- getCommercialPaperYearend(cp,"text")
#' summary(yearend)
#' }
getCommercialPaperYearend <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=5,
                                     columnName="Year-End",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpy"
  return(rv)
}

#' @title Extract the commercial paper outstanding (old) data.
#' @description Extracts the commercial paper outstanding (old) series from the commercial paper data set.
#' @inheritParams getCommercialPaperRates
#' @return Returns a list containing data frame for each series, class \code{usfrbcpw}.
#' @export
#' @seealso \code{\link{getCommercialPaper}},
#' \code{\link{getCommercialPaperRates}}, 
#' \code{\link{getCommercialPaperRatesOld}}, 
#' \code{\link{getCommercialPaperVolumes}}, 
#' \code{\link{getCommercialPaperOutstanding}}, 
#' \code{\link{getCommercialPaperYearend}},
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' outstanding <- getCommercialPaperOutstandingOld(cp,"text")
#' summary(outstanding)
#' }
getCommercialPaperOutstandingOld <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=6,
                                     columnName="Outstanding",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpw"
  return(rv)
}

#' @title Extract the commercial paper rates (old) data.
#' @description Extracts the commercial paper rates (old) series from the commercial paper data set.
#' @inheritParams getCommercialPaperRates
#' @return Returns a list containing data frame for each series, class \code{usfrbcpr}.
#' @export
#' @seealso \code{\link{getCommercialPaper}}, 
#' \code{\link{getCommercialPaperRates}}, 
#' \code{\link{getCommercialPaperVolumes}}, 
#' \code{\link{getCommercialPaperOutstanding}}, 
#' \code{\link{getCommercialPaperOutstandingOld}}, 
#' \code{\link{getCommercialPaperYearend}}, 
#' \code{\link{mergeSeries}}
#' @examples
#' \donttest{
#' cp <- getCommercialPaper()
#' rates <- getCommercialPaperRatesOld(cp,"text")
#' summary(rates)
#' }
getCommercialPaperRatesOld <- function(cp,progressBar="none") {
  rv <- extractCommercialPaperSeries(cp,
                                     seriesIndex=7,
                                     columnName="Rate",
                                     progressBar=progressBar)
  class(rv) <- "usfrbcpx"
  return(rv)
}
