#' Filedump logger
#'
#' @section Details:
#' This logger dumps the \code{new} dataset to file using the filewriter specified in \code{writer}.
#' The filenames are created automatically. The format is \code{dump-[nr]-[method].csv}, where \code{[nr]}
#' is a number that increases each time the logger is called, and \code{[method]} is the name of the
#' function calling \code{\link{write_log}}.
#' 
#'   
#' @param out \code{[character]} Directory name where files will be created. If no directory name is given,
#' a random directory will be created using \code{\link[base]{tempdir}} and the directory will be registered.
#' @param write \code{[function]} A function taking a dataset and a filename as input that is used to write 
#' data to file. By default, files are written in csv format.
#' 
#'
#' @return A function that accepts two mandatory arguments:
#' \itemize{
#' \item{\code{old} \code{[data.frame]} unmodified data. Only for compatability with \code{\link{write_log}}}
#' \item{\code{new} \code{[data.frame]} modified data. This dataset is written into the output directory.} 
#' }
#' 
#' @export
filedump <- function(out, write = function(x,file) write.csv2(x=x,file=file,row.names=FALSE)){
  if (missing(out)){
    out <- tempdir()
    logger_adddb("file-dump", db=out, close=function(...) invisible(NULL))
  }
  if (!file.exists(out)){
    warning(sprintf("The location %s could not be found.",out))
  }
  
  outdir <- out
  cntr <- 1
  structure(
    function(old, new,...){
      missing(old) # so 'old' gets used
      method <- as.character(sys.call(1)[[1]])
      fn <- sprintf("dump-%03d-%s.csv",cntr,method)
      write(new,file=fn)
      cntr <<- cntr + 1
    }
    , name="file-dump"
  )
}





