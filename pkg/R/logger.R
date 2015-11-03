
logger <- setRefClass("logger", fields=c(con="ANY"))

#' Call the loggers
#'
#' Get the registered loggers for a function and execute them.
#'
#' @param old Old dataset to pass to the logger(s)
#' @param new New dataset to pass to the logger(s)
#' @param ... Extra arguments passed to the loggers
#'
#' @section Details:
#' This function is the hook for programmers who want to 
#' enable their function to register for logging with the 
#' \code{data.log} package. This function does the following
#' things.
#' \enumerate{
#'   \item{It finds out what function called \code{write_log}.}
#'   \item{It asks the central logging registry what loggers are registered for the calling function.}
#'   \item{The loggers are called as follows \code{[logger]$log(old=old, new=new, method=method, ...)}}
#'   \item{The set \code{new} is returned, invisibly}
#' }
#' 
#' @seealso
#' 
#' \itemize{
#' \item{Explanation of the logging mechanism: \code{\link{add_logger}} }
#' \item{Loggers: \code{\link{shortlog}}, \code{\link{csvdump}}, \code{\link{simplediff}} }
#' }
#' 
#' @examples
#' # The following function multiplies the cell in the first row and column with two
#' # and asks the registered logger(s) (if any) to log the difference.
#' change <- function(df){
#'   .df <- df
#'   df[1,1] <- 2*df[1,1]
#'   writelog(old=.df, new=df)
#' } 
#' 
#'
#' @export 
write_log <- function(old, new, ...){
  fn <- as.character(sys.call(1)[[1]])
  loggers <- LOGREG$getloggers(fn)
  for ( lg in loggers ) lg$log(old=old, new=new, method=fn, ...)
  invisible(new)
}

#' Create a template logger
#' 
#' @param name \code{[character]} Name of the logger.
#' @param file \code{[character]} File to write the template in.
#'
#' @export
logger_template <- function(name,file=paste0("./",name,".R")){
  txt <- sub("{{{name}}}",name,lgtmpl(),fixed=TRUE)
  write(txt,file=file)
  message("Wrote a template logger named %s in %s.",file)
}


lgtmpl <- function(){
"
setRefClass({{{name}}}
    # add fields if necessary, to store extra info.
  , fields=list(con='ANY')
    # list of minimally mandatory methods
  , methods  = list(
    initialize = function(con){
      # initialize the connection, if necessary. Then:
      con <<- con
    }
  , close = function(){
      # call a function that closes the connection (if necessary).
      invisible(NULL)
    }
  , status = function(){
    # make this function return a short string stating
    # the object class and connection status (or value)
  }
  , log = function(old, new, method,...){
      # inspect the old and new value, and send logging info (including 'method')
      # to the connection. 
    }
  ) # end of methods
)
"
}