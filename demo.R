
devtools::load_all('pkg')

# a function that makes some changes.
change <- function(df){
  # store the old value
  .df <- df
  df[1,1] <- 2*df[1,1]
  # this is the 'hook' to log, if the function is registered.
  write_log(old=.df, new=df)
}

# let's add a logger to the logging registry
# This logger just writes a single line to screen.
lg <- add_logger(shortlog())

# let's assign the logger to a function
set_log(change, lg)

# check the registry
LOGREG$status()

w1 <- change(women)
w2 <- change(women)

