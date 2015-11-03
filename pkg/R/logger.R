
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
#'   \item{It finds out what function is called \code{write_log}.}
#'   \item{It asks the central logging registry what loggers are registered for the calling function.}
#'   \item{The loggers are called, passing the arguments \code{old=old, new=new, method=method}}
#'   \item{The set \code{new} is returned, invisibly}
#' }
#' 
#' @seealso \code{\link{csvdump}}
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


#' Create a simplediff instance.
#'
#'
#'
simplediff <- setRefClass("simplediff",contains="logger"
  , methods=list(
   initialize = function(file=":memory:"){
     con <<- simple_diff_db(file=file)
   }
   , close = function() DBI::dbDisConnect(con)
   , log = function(old, new, key=NULL, note=NULL){
        method <- as.character(sys.call(1)[[1]])
        tmstmp <- Sys.time()
        icol <- match(names(old),names(new),nomatch=0)
        A <- which(old != new[icol],arr.ind=TRUE)
        nlog <- nrow(A)
        diff <- data.frame(
          recnr = A[,1]
          , timestamp = rep(tmstmp, nlog)
          , key= if(is.null(key)) rep(NA_character_, nlog) else old[A[,1],key]
          , variable = names(old)[A[,2]]
          , old=format(old[A]) 
          , new=format(new[A])
          , method = rep(method,nlog)
          , note= if ( is.null(note) ) rep(NA_character_,nlog) else rep(note,nlog)
          , stringsAsFactors=FALSE
        )
        RSQLite::dbWriteTable(con, "log", diff,append=TRUE,row.names=FALSE) 
        new
   }
   , status = function(){
     stat <- if (dbIsValid(con)) "open" else "closed"
     sprintf("'simplediff' object with %s SQLite connection",stat)
   }
  )
)


# Create a database for the simple_diff logger
# 
# @param file [character]. Filename for the database. If no file is given, an in-memory database is created.
# 
# @return A DBI object, pointing to an RSQLite database containing a single table with the following columns.
# \itemize{
#   \item{  \code{recnr}     \code{[integer]}       Record number}
#   \item{ \code{timestamp}  \code{[integer]}       POSIXct integer representation of time}
#   \item{ \code{key}        \code{[character]}     Possibly, a key value identifying the record}
#   \item{ \code{variable}   \code{[character]}     Name of the variable that was changed}
#   \item{ \code{old}        \code{[character]}     Old value (formatted to string)}
#   \item{ \code{new}        \code{[character]}     New value (formatted to string)}
#   \item{ \code{method}     \code{[character]}     Name of the function that called the logger}
#   \item{ \code{note}       \code{[character]}     Optional remark or other information about the change}
# }
# 
# @seealso \code{\link{simple_diff}} for the logger that fills this database.
# 
# @export
simple_diff_db <- function(file= ":memory:" ){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=file)
  
  lgtab <- data.frame(
    recnr = integer(0)
    , timestamp = integer(0)
    , key = character(0)
    , variable = character(0)
    , old = character(0)
    , new = character(0)
    , method = character(0)
    , note = character(0)
  )
  
  tryCatch(dbWriteTable(con, "log", lgtab)
    , error=function(e){ dbDisconnect(con); stop(e)}
  )
  loc <- ifelse(file==":memory:", "in-memory logging database"
                , sprintf("logging database at %s\n",file))
  message(sprintf("Created %s",loc))

  con
}





