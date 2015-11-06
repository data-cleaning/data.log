
setRefClass("simplediff",contains="logger"
  , fields = list(level='numeric')  
  , methods = list(
      initialize = function(file, level){
        level <<- level
        con <<- dbConnect(SQLite(),file)
        # setup the database.
        dbSendQueries(con, sd_sql_tables())
      }
    , status = function(){
        stat <- if (dbIsValid(con)) "open" else "closed"
        sprintf("'simplediff' object with %s SQLite connection",stat)
    }
    , close = function(){
        dbDisconnect(con)
    }
    , log = function(dat, ref, method, key=NULL, note=NULL){
        if (missing(note)) note <- ""
        if (missing(key))
        vo <- names(dat)
        vn <- names(ref)
        added_var <- vo[!vo %in% vn]
        removed_var <- vn[!vn %in% vo]
        kept_var <- vo[vo %in% vn]
        A <- which(dat[kept_var] != ref[kept_var], arr.ind=TRUE)
        # TODO: add logging of added/removed columns
        lg <- data.frame(
          timestamp      = Sys.time()
          , old          = format(ref[kept_var][A], digits=16)
          , new          = format(dat[kept_var][A], digits=16)
          , recordid     = store_pk(con,"record",  if (is.null(key)) A[,1] else dat[A[,1],key] ) 
          , variableid  = store_pk(con, 'variable', kept_var[A[,2]])
          , methodid     = store_pk(con,'method',method)
          , noteid       = if(is.null(note)) NA else store_pk(con,'note',note)
         )
        dbBegin(con)
        on.exit(dbCommit(con))
        qry <- sprintf("INSERT INTO log(%s) VALUES (?,?,?,?,?,?,?)"
                       , paste0(names(lg),collapse=","))
        dbGetPreparedQuery(con,qry,lg)
    }
    , get_log = function(){
        qry <- "select logid, timestamp, record, variable, old, new, method, note 
                FROM log JOIN record JOIN variable JOIN note JOIN method"
        lg <- dbGetQuery(con,qry)  
        within(lg, timestamp <- .POSIXct(lg$timestamp))
      }
  ) # end of methods 
)


#' Simple difference logger
#' 
#' @param file Filename for the \code{SQLite} database storing the log (will be 
#'   created if necessary). By default, an in-memory database is created.
#' @param level logging level, higher means more logging (see details).
#' 
#' @section Details:
#' The \code{simplediff} logger compares two rectangular data sets (data.frames)
#' (\code{dat} (new dataset) and \code{ref} (old dataset)), and assumes that
#' \itemize{
#'   \item{the record number and order is the same in \code{old} and \code{new}.}
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
  new("simplediff", file=file, level=level)
}




# SQL stuff ----------------------------------------------
sd_sql_tables <- function(){ 
"
CREATE TABLE record(
  recordid    INTEGER PRIMARY KEY, 
  record      TEXT UNIQUE -- A text key
);
CREATE TABLE variable(
  variableid     INTEGER PRIMARY KEY,
  variable       TEXT UNIQUE
);
CREATE TABLE method(
  methodid    INTEGER PRIMARY KEY,
  method      TEXT UNIQUE
);
CREATE TABLE note(
  noteid      INTEGER PRIMARY KEY,
  note        TEXT UNIQUE
);
CREATE TABLE log(
  logid        INTEGER PRIMARY KEY,
  timestamp    INTEGER,
  old          TEXT,
  new          TEXT,
  recordid     INTEGER,
  variableid   INTEGER,
  methodid     INTEGER,
  noteid       INTEGER,
  FOREIGN KEY(recordid) REFERENCES record(recordid),
  FOREIGN KEY(variableid) REFERENCES variable(variableid),
  FOREIGN KEY(methodid) REFERENCES method(methodid),
  FOREIGN KEY(noteid) REFERENCES note(noteid)
);"
}

# help function since RSQLite don't wanna eat more than one SQL statement.
dbSendQueries <- function(con, sql){
  s <- paste0(strsplit(sql,";")[[1]],";")
  lapply(s, function(x) dbGetQuery(con, x) )
}

# Store unique values in the simplediff database.
# Return the corresponding primary keys.
store_pk <- function(con, name, value){
  value <- as.character(value)
  name <- as.character(name)
  if (length(value) > 0){
    store_multi_pk(con, name, value)
  } else {
    qry <- sprintf("INSERT OR IGNORE INTO %s (%s) VALUES('%s')",name, name,value)
    dbSendQuery(con,qry)
    dbGetQuery(con,"SELECT last_insert_rowid()")[,1]
  }
}


# Store multiple values.
# return a vector of primary keys.
store_multi_pk <- function(con,name,values){
  values <- data.frame(x=values)  
  puts <- sprintf("INSERT OR IGNORE INTO %s (%s) VALUES (?) ", name, name)
  gets <- sprintf("SELECT %sid FROM %s WHERE %s = ?", name,name,name)
  dbBegin(con)
  on.exit(dbCommit(con))
  dbGetPreparedQuery(con, puts, values)
  dbGetPreparedQuery(con, gets, values)[,1]
}



