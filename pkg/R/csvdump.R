#' @include logger.R
NULL

#' Dump copy of data to a csv file
#'
#' This logger enables you to dump a csv-copy of a dataset. Filenames are numbered automatically.
#' 
#' @section Constructor:
#' 
#' \code{csvdump(dir, format)}
#' 
#' @field dir \code{[character]} Path to a directory to dump files into. 
#'    By default, \code{\link[base]{tempdir}()/output} is used. If the directory does not exist,
#'    it will be created when de logger is created.
#'    
#' @field format \code{[character]} Either \code{"csv2"} (default) or \code{"csv"}. The 
#' \code{csv} format to use.
#'
#' @section Hook:
#' When \code{\link{write_log}} is called, the data in argument \code{new} is written to 
#' a file named 
#' 
#' \code{   dump-[nnn]-[method].csv},
#' 
#' where \code{[nnn]} increases every time \code{write_log} is called and \code{method} is the
#' name of the function calling \code{write_log}.
#'
#' @export
csvdump <- setRefClass("csvdump"
  , contains = "logger"
  , fields = list(writer="function", n="numeric", format="character") 
  , methods = list(
      initialize = function(dir=file.path(tempdir(),"output"), format=c("csv2","csv")){
        if (!file.exists(dir)){
          dir.create(dir,recursive=TRUE)
        }
        con <<- dir
        format <<- match.arg(format)
        writer <<- if (format == "csv2"){ 
          function(x,file) write.csv2(x=x,file=file,row.names=FALSE)
        } else {
          function(x,file) write.csv(x=x,file=file,row.names=FALSE)
        }
      }
    , close = function() invisible(TRUE)
    , status = function(){
       sprintf("'%s' object writing to %s",class(.self),con)
    }
    , log = function(old, new, method, ...){
        missing(old) # just so this argument is used.
        fl <- sprintf("dump-%3d-%s.csv",n,method)
        writer(new, file.path(dir,fl))
        n <<- n + 1
    }
  )
)


