
# Internal class that administrates what functions are logged, and if so, by what
# logger or loggers.
#
# TODO: addlogger should do some basic check on the logger object
# (e.g. do the required methods exist)
#
logregister <- setRefClass("logregister"
    , fields=list(
        loggers    = "environment"  # hold loggers, named.
      , functions  = "environment"  # hold function names, with their loggers.
      , logcount   = "numeric"      # count loggers per class
      )
    , methods=list(
      addlogger = function(logger=simplediff()){
        # stops if 'logger' is not valid.
        is_logger(logger)
        cname = class(logger)[[1]]
        logcount[cname] <<- if (is.na(logcount[cname])) 1L else logcount[cname] + 1L
        regname <- sprintf("%s%02d",cname,logcount[cname]) 
        loggers[[regname]] <<- logger
        msg <- sprintf("Registered logger of class %s under %s",cname,regname)
        message(msg)
        regname
      }
    , rmlogger = function(logreg){
        if (is.null(logreg)){
          rm(list=ls(envir=loggers), pos=loggers)
        } else {
          stopifnot( is.character(logreg) )
          rm(logreg, envir=loggers)
        }
    }
    , addfun = function(fun,logreg){
        stopifnot(is.character(fun),is.character(logreg))
        if (!logreg %in% ls(envir=loggers)){
          stop("Logger %s not registered.",logreg)
        }
        fn <- fun
        functions[[fn]] <<- if (fn %in% ls(envir=functions)) c(functions[[fn]],logreg) else logreg
        msg <- sprintf("Function '%s' now logged by %s", fn, logreg)
        message(msg)
        logreg
    }
    , rmfun = function(fun, logreg=NULL){
        stopifnot(is.function(fun))
        fn <- as.character(substitute(fun))
        if (is.null(logreg)){ # remove all loggers for fun
          rm(fn,envir=functions)
        } else {
          stopifnot(is.character(logreg))
          if ( !logreg %in% ls(envir=loggers) ){
            stop('%s is not a registered logger',logreg)
          }
          functions[[fn]][logreg] <<- NULL
          if (length(functions[[fn]])==0){
            rm(fn,envir=functions)
          }
            
        }
      }
    , clear = function(){
      for (lg in ls(loggers)){
        loggers[[lg]]$close()
      }
      rm(list=ls(envir=loggers),pos=loggers)
      logcount <<- numeric()
      rm(list=ls(envir=functions),pos=functions)
    }
    # get loggers (list) for a certain function.
    , getloggers = function(fun){
        stopifnot(is.character(fun))
        lapply(functions[[fun]], get, envir=loggers)
    }
    , status = function(){
      cat("Registered loggers:")
      for (x in ls(envir=loggers)){
        cat( sprintf("  \n%15s [%s]",x,loggers[[x]]$status()) )
      }
      cat("\nLogged functions:")
      for (x in ls(envir=functions) ){
        cat( sprintf("\n%15s - logged by %s",x,paste(functions[[x]],collapse=", ")) )
      }
      cat("\n")
    }
  ) # end method list
)

# internal logging register
LOGREG <- logregister(loggers=new.env(),logcount=numeric(), functions=new.env())


#' Check if an object is acceptable as a logger.
#' @param x An R object. Candidate logger
#' 
#' @export
check_logger <- function(x){
  xnames <- ls(envir = x$.refClassDef@refMethods)
  errors <- character(0)
  warnings <- character(0)
  
  if ( !"close" %in% xnames ){
    errors <- c(errors, "Object has no 'close' method")
  }
  
  # check methods
  if ( !"status" %in% xnames ){
    errors <- c(errors, "Object has no 'status' method ")
  } else if ( !is.function(x$status) ){
    errors <- c(errors,"'status' is not a function")
  } else if ( !is.character(x$status()) ){
    errors <- c(errors,"output of 'status' is not of type 'character'")
  } else if ( nchar(x$status()) > 120 ){
    warnings <- c(warnings, "Lengthy output of 'status' (>120 characters)")
  }
  
  list(errors=errors, warnings = warnings)
}


is_logger <- function(x){ 
  cl <- check_logger(x)$errors
  if (length(cl) == 0) 
    invisible(TRUE)
  else 
    stop(simpleError(paste(cl,collapse="\n"),call=sys.call(1)[[1]]))
}

#' Add or remove loggers to the registry
#'
#' @param logger  A \code{logger}. By default, \code{\link{shortlog}} is used.
#' @param logreg \code{[character]}. The name of a registered logger.
#' 
#' @section Details:
#' 
#' \code{add_logger} adds a logger to the registry. It can henceforth be found
#' by the name returned by it. An overview of registered loggers is printed by
#' \code{\link{logreg_status}}.
#' 
#' \code{rm_logger} removes a logger from the registry. If the argument \code{logreg} is
#' not specified, all loggers are removed. Note that possible open connections are not
#' closed by this command.
#' 
#' \code{get_logger} get a reference to a logger object from the registry.
#' 
#' \code{get_con} retrieve only the connection field of a logger object (this is what you usually want).
#'
#' @section Loggers:
#' 
#' The purpose of a \emph{data logger} is to store useful information about changes in the
#' data without perturbing the flow of data through a script or pipeline. Examples
#' of loggers that come with the package are
#' \itemize{
#' \item{\code{\link{shortlog}} A single-line logger that prints simple
#' information to the screen (or another connection)}
#' \item{\code{\link{csvdump}} A logger that dumps a copy of the dataset to csv in a
#' predefined directory.}
#' \item{\code{\link{simplediff}} A logger that writes changes to a dataset to a \code{SQLite} database
#' that can later be queried by the user.}
#' }
#' 
#' 
#' In this package, a \code{logger} is basically a dressed-up connection. It is a reference class object
#' that has at least the following field.
#' \itemize{
#' \item{\code{con}: A connection or other object that allows the logger to
#' write and the user to find the logging data.}
#' }
#' It must also have the following methods
#' \itemize{
#' \item{close()} A function that closes the connection. If the connection does not need to be closed,
#' (e.g. because it is a directory), it should be a simple function doing nothing.
#' \item{\code{log(old,new,method,...)}: A function that eats two versions of a data set (old and new)
#' and a string that tells it what function is calling \code{log}. There is no return value,
#' but the function writes something to the connection specified by \code{con}. Either of the arguments
#' may be ignored by \code{log}, but it must at least accept these three arguments, since the
#' function \code{\link{write_log}} will pass all three.}
#' \item{\code{status()}: A no-argument function that returns a short string stating the
#' type of logger and the status of the connection (open, closed)}
#' }
#' 
#' The idea of this package is to be extendable. That is, any package or script
#' may extend this package by writing a logger that can be registered by this
#' package. The documentation in this package is probably not enough to get you
#' started. You can browse the 
#' \href{https://github.com/data-cleaning/data.log/blob/master/pkg/R/shortlog.R}{source
#' code} for inspiration of follow the tutorial in the following vignette:
#' 
#' \code{  packageVignette("Adding loggers",package="data.log")}
#' 
#' Finally, the function \code{\link{logger_template}} creates a file with a
#' template for you to fill in.
#' 
#' 
#' @return 
#' \itemize{
#' \item{\code{add_logger}: \code{[character]}, the name of the logger created in the logregistry.}
#' \item{\code{rm_logger}: \code{NULL} (invisibly)}
#' \item{\code{get_logger}: An RC object representing a logger.}
#' \item{\code{get_con}: The \code{connection} field of a registered logger. 
#'   The type depends on the nature of the logger.}
#' }
#' @export 
add_logger <- function(logger=shortlog()){
  LOGREG$addlogger(logger)
}


#' @rdname  add_logger
#' @export
rm_logger <- function(logreg){
  if (missing(logreg)) logreg <- NULL
  LOGREG$rmlogger(logreg)
}

#' @rdname add_logger
#' @export
get_logger <- function(logreg){
  stopifnot(is.character(logreg))
  LOGREG$loggers[[logreg]]
}

#' @rdname add_logger
#' @export
get_con <- function(logreg){
  stopifnot(is.character(logreg))
  LOGREG$loggers[[logreg]]$con
}

#' Add of remove functions to the registry
#'
#' @param fun \code{[function]} to add or remove.
#' @param logreg \code{[character]} Name of a registered logger (see details).
#' 
#' @section Details:
#' The central registry maps functions to loggers. The map is in principle n:m.
#' A single function can be traced by mutliple loggers, and each logger can 
#' trace one or more functions.
#' 
#' \code{set_log(fun,logreg)} hooks a function to a logger. With 
#' \code{unset_log(fun,logreg)}, a function is unhooked from that logger. If 
#' \code{logreg} is left unspecified, the function is unhooked from all loggers.
#' 
#' \code{logreg_clear()} unhooks all functions from all loggers,
#' closes all logging connections, and unregisters all loggers.
#' 
#' \code{logreg_status()} prints an overview of currently registered loggers
#' and logged functions.
#' 
#' 
#' @return 
#' \itemize{
#' \item{For \code{set_log} \code{[character]}: The name of the logger in the registry (invisibly)}
#' \item{For \code{unset_log} \code{[character]}: \code{NULL} (invisibly)}
#' \item{For \code{logreg_clear}\code{[character]}:\code{NULL} (invisibly)}
#' }
#' 
#' @example ../examples/logging.R
#'
#' @export
set_log <- function(fun,logreg){
  invisible(LOGREG$addfun(as.character(substitute(fun)),logreg))
}


#' @rdname set_log
#' @export
unset_log <- function(fun,logreg){
  fn <- as.character(substitute(fun))
  logreg <- if (!missing(logreg)) logreg # else NULL
  invisible(LOGREG$rmfun(fn,logreg))
}



#' @rdname set_log
#' @export
logreg_clear <- function(){
  LOGREG$clear()
}


#' @rdname set_log
#' @export
logreg_status <- function(){
  LOGREG$status()
}

