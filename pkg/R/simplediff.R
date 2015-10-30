
#' Create a database for the simple_diff logger
#' 
#' @param file [character]. Filename for the database. If no file is given, an in-memory database is created.
#' 
#' @return A DBI object, pointing to an RSQLite database containing a single table with the following columns.
#' \itemize{
#'   \item{  \code{recnr}     \code{[integer]}       Record number}
#'   \item{ \code{timestamp}  \code{[integer]}       POSIXct integer representation of time}
#'   \item{ \code{key}        \code{[character]}     Possibly, a key value identifying the record}
#'   \item{ \code{variable}   \code{[character]}     Name of the variable that was changed}
#'   \item{ \code{old}        \code{[character]}     Old value (formatted to string)}
#'   \item{ \code{new}        \code{[character]}     New value (formatted to string)}
#'   \item{ \code{method}     \code{[character]}     Name of the function that called the logger}
#'   \item{ \code{note}       \code{[character]}     Optional remark or other information about the change}
#' }
#' 
#' @seealso \code{\link{simple_diff}} for the logger that fills this database.
#' 
#' @export
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


#' Create a simple logger for changes in data
#'
#' @param db \code{[SQLiteConnection]} to a database created with \code{\link{simple_diff_db}}. If
#' \code{db} is not given, it is checked whether a database named \code{"simple-diff"} has been 
#' registered in the logging registry. If this is the case, this database will be used. If not,
#' a new one will be created and registered under that name.
#' 
#'  
#' @return A logging function.
#' A function with the signature 
#'   \itemize{
#'   \item{old: \code{[data.frame]} data before change}
#'   \item{new: \code{[data.frame]} data after change}
#'   \item{key: \code{[character], (NULL)} name of identifying column}
#'   \item{note: \code{[character],(NULL)} a note for a certain logging action}
#' }
# Here, 'old' and 'new' must have the same order of records, and every column in 'old' must also occur in 'new'
#' 
#' @seealso \code{\link{set_log}} for examples.
#' @export
simple_diff <- function(db){ 
  
  if (missing(db)){
    if (!logger_hasdb("simple-diff")){
      logger_adddb("simple-diff", db=simple_diff_db(), close=RSQLite::dbDisconnect )
    }
    con <- logger_getdb("simple-diff")
  } else {
    stopifnot(inherits(db,"SQLiteConnection"))
    con <- db
  }

  
  structure( 
    function(old, new, key=NULL, note=NULL){
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
      RSQLite::dbWriteTable(con, "log", diff,append=TRUE) 
    }
    , name="simple-diff"
  )
  
}


