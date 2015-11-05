[![Build Status](https://travis-ci.org/data-cleaning/data.log.svg?branch=master)](https://travis-ci.org/data-cleaning/data.log)
[![Coverage Status](https://coveralls.io/repos/data-cleaning/data.log/badge.svg?branch=master&service=github)](https://coveralls.io/github/data-cleaning/data.log?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/data.log)](http://cran.r-project.org/package=data.log/)
[![Downloads](http://cranlogs.r-pkg.org/badges/data.log)](http://www.r-pkg.org/pkg/data.log) 

# data.log

Logging framework for tracking changes in datasets


To try:
```
git clone https://github.com/data-cleaning/data.log
```

From R do

```
setwd("data.log")
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
logreg_status()

w1 <- change(women)
w2 <- change(women)
```


