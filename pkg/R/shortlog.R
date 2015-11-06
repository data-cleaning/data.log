setRefClass("shortlog"
    , contains = "logger"
    , methods  = list(
      initialize = function(con=stdout()){
        con <<- con
      }
    , close = function() invisible(NULL)
    , status = function(){
        sprintf("'%s' object logging to %s",class(.self)[[1]],class(con)[[1]])
    }
    , log = function(dat, ref, method,...){
        ts <- timestamp(prefix="",suffix="",quiet=TRUE)
        i <- match(names(ref),names(dat),nomatch = 0)
        n <- sum(ref != dat[i])
        p <- prod(dim(ref))
        r = n/p * 100
        txt <- sprintf("%s [%s] modified %d out of %d cells (%04.1f%%)",ts,method,n,p,r)
        cat(txt,file=con)
      }
    ) # end of methods
)
                        
#' Single-line textlogger.
#'
#' @param con Connection to send output to. If no connection is given, \code{stdout} is used.
#' 
#' @section Hook:
#' 
#' When \code{\link{write_log}} is called, a single-line summary is printed to \code{con}.
#'
#' @return An object of class \code{shortlog}.
#'
#' @export
shortlog <- function(con=stdout()) new("shortlog",con=con)


