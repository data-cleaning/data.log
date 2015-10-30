# A class for storing which function is logged by what logger.
# There is also room to register logging databases, but this is
# not mandatory: the connection to the logging output must be
# in the logger's closure.
#
logregistry <- setRefClass("logregistry"
  , fields = list(.db = "environment", .loggers = "environment")
  , methods = list(
    # add database connection.
    add_db = function(name, db, close){
      if ( .self$has_db(name) ){
        stop(sprintf("db '%s' exists, skipping\n",name))
        invisible(FALSE)
      }
      .self$.db$db[[name]] <- db
      .self$.db$closers[[name]] <- close
      message(sprintf("Registered logging database under '%s'",name))
      invisible(TRUE)
    }
    # retrieve database connection stored under 'name'
    , get_db = function(name){
      if (is.null(.self$.db$db[[name]])){
        warning(sprintf("Unregistered logging database %s",name))
        return(NULL)
      }
      .self$.db$db[[name]]
    }
    # close a database
    , close_db = function(name){
      if ( !.self$has_db(name) ){
        warning(sprintf("No database with name %s registered",name))
      }
      .self$.db$closers[[name]](.self$.db$db[[name]])
    }
    # clear all loggers and databases
    , clear = function(){
      for ( nm in names(.self$.db$db) ) .self$close_db(nm)
      .self$.db <- as.environment(list(db=list(),closers=list()))
      .self$.loggers <- new.env()
    }
    # add logger under 'name'. Multiple loggers may be stored.
    , add_logger = function(name, fun){
      if (!is.null(.self$.loggers[[name]])){
        for ( f in .self$.loggers[[name]]){
          if ( identical(f,fun)){
            warning(sprintf("This function is already registered as logger for %s. Skipping.",name))
            invisible(FALSE)
          }
        }
      }
      .self$.loggers[[name]] <- c(.loggers[[name]], fun)
      message(sprintf("Registered '%s' for logging %s",name, sprintf("with '%s'",attr(fun,'name'))) )
      invisible(TRUE)
    }
    # remove all loggers stored under 'name'
    , remove_logger = function(name){
      if (is.null(.self$.loggers[[name]])){
        invisible(TRUE)
      }
      do.call("rm",list(name,envir=.self$.loggers))
      invisible(TRUE)
    }
    # check existence of a database
    , has_db = function(name) name %in% names(.self$.db$db)
    # retrieve the list of loggers
    , get_loggers = function(name){
      if (name %in% names(.self$.loggers)){
        .self$.loggers[[name]]
      } else {
        # return list with dummy logger
        list(function(...) invisible(TRUE))
      }
    }
    # status report
    , status = function(){
      dbnames <- names(.self$.db$db)
      dbnames <- if (length(dbnames) == 0 ) "none" else dbnames 
      cat(sprintf("Registered logging databases: %s\n",paste0(dbnames,collapse=", ")))
      
      fnnames <- names(.self$.loggers)
      statstr <- if (length(fnnames)==0){ 
        "none"
      } else {
        sapply(fnnames, function(x){ 
           loglabel = paste0(sapply(.self$.loggers[[x]],attr,"name"),collapse=", ")
           sprintf("\n  %s %s",x,sprintf(" [with %s]",loglabel))
        })
      }
      cat(sprintf("Functions registered for logging: %s",paste(statstr,collapse="")))
      
    }
  )
)


LOGREG <- logregistry(
  .db=as.environment(list(db=list(), closers=list()))
  ,.loggers=new.env())


#' Register a function for logging.
#'
#' @param fun The function to be registered.
#' @param logger The logging function to use
#'
#' @export
set_log <- function(fun, logger=simple_diff()) {
  fn <- as.character(substitute(fun))
  LOGREG$add_logger(fn, logger)
}

#' @describeIn set_log Remove all loggers for a function
unset_log <- function(fun){
  fn <- as.character(substitute(fun))
  LOGREG$remove_logger(fn)
}


#' Clear the logging register
#'
#' Remove all functions from the register so nothing is logged anymore.
#' If any databases are registered, close the connection and remove them from the register.
#' 
#' @return Nothing.
logger_clear <- function(){
  LOGREG$clear()
}

#' Print status of logging register.
#'
#' Print and overview of registered databases and functions registered for logging.
#' If the loggers are named, all loggers for a function are printed as well.
#' 
#' @return Nothing.
#' @export
logger_status <- function(){
  LOGREG$status()
}

#' Get connection to registered database.
#'
#' @param dbname \code{[character]} name of a registered database.
#'
#' @return A connection to the registered database. Depending on the logger, this may
#' be the name of a file or directory.
#' @export
logger_getdb <- function(dbname){
  LOGREG$get_db(dbname)
}

#' Register a logging database
#'
#' @param name \code{[character]} Name under which the database will be registered.
#' @param db Object representing a connection to a logging database. For some loggers this
#' may be the location of a file or directory.
#' @param close \code{[function]}. A function closing the connection. Will be called by
#' \code{\link{logger_clear}}, for instance.
#' @export
logger_adddb <- function(name, db, close){
  LOGREG$add_db(name,db,close) 
}


#' Use a registered logger.
#' 
#' Call this function from within a data-modifying function to generate logging data and write it.
#' 
#' @param old \code{[data.frame]} The dataset before treatment.
#' @param new \code{[data.frame]} The dataset after treatment.
#' @param ... extra parameters, to be pass to the logger.
#' 
#' @return Boolean (invisible) indicating whether logging was successful.
#' @export 
write_log <- function(old, new, ...){
  fn <- as.character(sys.call(1)[[1]])
  L <- LOGREG$get_loggers(fn)
  sapply(L,function(h) tryCatch(h(old=old, new=new, ...), error=function(e){ warning(e); FALSE},finally=TRUE ))
}

#' Check wether a database is registered under a certain name
#' @param dbname name of the database
#' @export
#' @return Logical
logger_hasdb <- function(dbname){
  LOGREG$has_db(dbname)
}


