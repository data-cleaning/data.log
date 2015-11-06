

context("Default loggers")

change <- function(df){
  old <- df
  df[1,1] <- 2*df[1,1]
  write_log(df, old)
}

# crash test dummies
test_that("loggers work",{
  logreg_clear()
  logreg_shutup()
  lg1 <- add_logger(simplediff())
  lg2 <- add_logger(shortlog())
  lg3 <- add_logger(csvdump())
  set_log(change,lg1)
  set_log(change,lg2)
  set_log(change,lg3)
  w1 <- change(women)
  logreg_clear() 
})

