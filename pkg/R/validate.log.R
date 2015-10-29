#' Logging framework for data cleaning
#' 
#' 
#' 
#'
#'    
#' @docType package
#' @name validate.log
#' @aliases package-validate validate
#' @import methods, settings, RSQLite
NULL


logregistry <- setRefClass("logregistry"
  , fields = list(.db = "list", .loggers = "list")
  , methods = list(
    add_db = function(name,db){
      if ( !is.null(.self$.db[[name]]) ){
        stop(sprintf("db '%s' exists, remove first\n",name))
      }
      .self$.db[[name]] <- db
      message(sprintf("Registered logging database under '%s'",name))
      invisible(TRUE)
    }
    , add_logger = function(name, fun){
      if (!is.null(.self$.loggers[[name]])){
        stop(sprintf("logger %s exists, unregister first",name))
      }
      .self$.loggers[[name]] <- fun
      message(sprintf("Registered %s for logging %s",name, sprintf("with '%s'",attr(fun,'name'))) )
      invisible(TRUE)
    }
    , has_db = function(name) name %in% names(.self$.db)
  )
)


LOGREG <- logregistry(.db=list(),.loggers=list())


#' Register a function for logging.
#'
#' @param fun The function to be registered.
#' @param logger The logging function to use
#'
#' @export
register <- function(fun, logger=simple_logger()) {
  fn <- as.character(substitute(fun))
  LOGREG$add_logger(fn, logger)
}


#' Create a new default logging database
#' 
#' @param file [character]. Filename for the database. If no file is given, an in-memory database is created.
#' 
#' @return A DBI object, pointing to an RSQLite database.
#' @export
new_logdb <- function(file= ":memory:" ){
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=file)
  
  lgtab <- data.frame(
    reckey = integer(0)
    , varkey = integer(0)
    , methkey = integer(0)
    , notekey = integer(0)
    , old = character(0)
    , new = character(0)
  )
  
  methtab <- data.frame(
    methkey = integer(0)
    , method = character(0)
  )
  
  rectab <- data.frame(
    reckey = integer(0)
    , id = character(0)
  )
  
  notetab <- data.frame(
    notekey = integer(0)
    , note = character(0)
  )
  tryCatch({
    dbWriteTable(con, "log", lgtab)
    dbWriteTable(con, "method", methtab)
    dbWriteTable(con, "record", rectab)
    dbWriteTable(con, "notes", notetab)
  }
    , error=function(e){ dbDisconnect(con); stop(e)}
  )
  loc <- ifelse(file==":memory:", "in-memory logging database"
                , sprintf("logging database at %s\n",file))
  message(sprintf("Created %s\n",loc))
  
  con
}


#' Create a simple logger for changes in data
#'
#' @param db The name of a registred database. If no such database
#'  exists, a default in-database log wil be used.
#'  
#' @return A logging function.
#' 
#' @export
simple_logger <- function(db="default"){
  .db <- db
  if ( !LOGREG$has_db(.db) ){
    LOGREG$add_db(.db, new_logdb())
  }
 
  structure( 
    function(old, new, key, note){
      # code to add difference and timestamp to .db
    }
    , name="simple logger"
  )
}


# DEMONSTRUCTION
# library(RSQLite)
# register(mean)






