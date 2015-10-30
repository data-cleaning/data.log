
library(RSQLite)
devtools::load_all('pkg')

change <- function(df){
  df.old <- df
  df[1,1] <- 2*df[1,1]
  write_log(old=df.old,new=df,note="fucked up")
}

# use the default 'simple-diff' logger.
set_log(change)
logger_status()
w <- change(women)
logdata <- dbReadTable(logger_getdb("simple-diff"),"log")

# close registered databases.  
logger_clear()



set_log(change,logger=filedump())
w <- change(women) 
logger_getdb('file-dump')

# check wheter a file was written
dir(logger_getdb("file-dump"))
logger_clear()

## use a custom directory
set_log(change, logger=filedump(out="./fiets"))
w <- change(women)
dir("./fiets")
unset_log(change)
