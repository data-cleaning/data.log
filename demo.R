
devtools::load_all('pkg')


#library(data.log)
change <- function(df){
  .df <- df
  df[1,1] <- 2*df[1,1]
  write_log(old=.df,new=df)
}

add_logger(shortlog())

set_log(change,"shortlog01")

LOGREG$status()


w1 <- change(women)

LOGREG$functions[["change"]]

