#' @include logger.R
NULL

setRefClass("csvdump"
  , contains = "logger"
  , fields = list(writer="function", n="numeric", format="character") 
  , methods = list(
      initialize = function(dir, format){
        if (!file.exists(dir)){
          dir.create(dir,recursive=TRUE)
        }
        n <<- 0
        con <<- dir
        format <<- format
        writer <<- if (format == "csv2"){ 
          function(x,fl) write.csv2(x=x, file=file.path(.self$con,fl), row.names=FALSE)
        } else {
          function(x,fl) write.csv(x=x, file=file.path(.self$con,fl), row.names=FALSE)
        }
      }
    , close = function() invisible(TRUE)
    , status = function(){
       sprintf("'%s' object writing to %s",class(.self),con)
    }
    , log = function(dat, ref, method, ...){
        missing(ref) # just so this argument is used.
        fl <- sprintf("dump-%3d-%s.csv",n,method)
        writer(x=dat, fl=fl)
        n <<- n + 1
    }
  )
)

#' Dump copy of data to a csv file
#'
#' This logger enables you to dump a csv-copy of a dataset. Filenames are numbered automatically.
#' 
#' 
#' @param dir \code{[character]} Path to a directory to dump files into. 
#'    By default, \code{\link[base]{tempdir}()/output} is used. If the directory does not exist,
#'    it will be created when de logger is created.
#'    
#' @param format \code{[character]} Either \code{"csv2"} (default) or \code{"csv"}. The 
#' \code{csv} format to use.
#'
#' @section Hook:
#' When \code{\link{write_log}} is called, the data in argument \code{dat} is written to 
#' a file named 
#' 
#' \code{   dump-[nnn]-[method].csv},
#' 
#' where \code{[nnn]} increases every time \code{write_log} is called and \code{method} is the
#' name of the function calling \code{write_log}.
#'
#' @export
csvdump <- function(dir=file.path(tempdir(),"output"), format=c("csv2","csv")){
  stopifnot(is.character(dir))
  format <- match.arg(format)
  new("csvdump",dir=dir,format=format)
}
