
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
lg <- add_logger(simplediff())

# let's assign the logger to a function
set_log(change, lg)

# check the registry
logreg_status()


w1 <- change(women)
w2 <- change(w1)


sd <- get_logger(lg)
sd$get_log()

logreg_clear()
logreg_status()
