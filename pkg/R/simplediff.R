
setRefClass("simplediff",contains="logger"
  , fields = list(level="numeric")
  , methods=list(
   initialize = function(file, level){
     stopifnot( is.character(file) )
     con <<- simple_diff_db(file=file)
     level <<- level
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

#' Simple difference logger
#' 
#' @param file Filename for the \code{SQLite} database storing the log (will be 
#'   created if necessary). By default, an in-memory database is created.
#' @param level logging level, higher means more logging (see details).
#' 
#' @section Details:
#' The \code{simplediff} logger compares two rectangular data sets (data.frames)
#' (\code{old} and \code{new}), and assumes that
#' \itemize{
#'   \item{all columns in \code{old} also appear in \code{new};}
#'   \item{the record order is the same in \code{old} and \code{new}.}
#' }
#' 
#' It is usefull for situations where a dataset is transformed incrementally, 
#' such as in stepwise data cleaning processes.
#' 
#'
#' 
#' @section Logging levels:
#' 
#' Level 1: only count number of altered cells.
#' 
#' Level 2: split in possible transformations: full-full, full-full', empty-full, full-empty, empty-empty. 
#' 
#' Level 3: full log report
#' 
#' @export
simplediff <- function(file=":memory:", level=1){
  new("simplediff",file=file,level=level)
}
  
  
  
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



