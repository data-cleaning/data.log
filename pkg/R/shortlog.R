#' Single-line textlogger.
#'
#' @field con Connection to send output to. If no connection is given, \code{stdout} is used.
#' 
#' @section Constructor:
#' 
#' \code{   shortlog(con)}
#' 
#' @section Hook:
#' 
#' When \code{\link{write_log}} is called, a single-line summary is printed to \code{con}.
#'
#' @export 
shortlog <- setRefClass("shortlog"
    , contains = "logger"
    , methods  = list(
      initialize = function(con=stdout()){
        con <<- con
      }
    , close = function() invisible(NULL)
    , status = function(){
        sprintf("'%s' object logging to %s",class(.self)[[1]],class(con)[[1]])
    }
    , log = function(old, new, method,...){
        ts <- timestamp(prefix="",suffix="",quiet=TRUE)
        i <- match(names(old),names(new),nomatch = 0)
        n <- sum(old != new[i])
        p <- prod(dim(old))
        r = n/p * 100
        txt <- sprintf("%s [%s] changed %d out of %d cells (%04.1f%%)",ts,method,n,p,r)
        cat(txt,file=con)
      }
    ) # end of methods
)
                        


