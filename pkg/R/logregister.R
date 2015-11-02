
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
    , addfun = function(fun,logreg){
        stopifnot(is.function(fun),is.character(logreg))
        if (!logreg %in% ls(envir=loggers)){
          stop("Logger %s not registered.")
        }
        fn <- as.character(substitute(fun))
        functions[[fn]] <<- if (fn %in% ls(envir=functions)) c(functions[[fn]],logreg) else logreg
        msg <- sprintf("Function '%s' now logged by %s", fn, logreg)
        message(msg)
        logger
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
        lg$close()
      }
      rm(list=ls(envir=loggers),pos=loggers)
      logcount <<- numeric()
      rm(list=ls(envir=functions),pos=functions)
    }
    # get loggers (list) for a certain function.
    , getloggers = function(fun){
        stopifnot(is.character(fun))
        lapply(functions[[fn]], lgnames, get, envir=loggers)
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
#' @param err Boolean toggle: emit error if x is not a valid logger?
#' @param warn Boolean toggle: emit warning if x is a valid logger but?
#' 
#' @export
check_logger <- function(x){
  xnames <- ls(x)
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


