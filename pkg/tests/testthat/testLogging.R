
#devtools::load_all('pkg')
#library(testthat)

context("Logger administration")

# the environment logger

setRefClass("envlog"
  , fields = list(con="environment")
  , methods= list(
    close = function() rm(list=ls(con),envir = con)
    , status = function() sprintf("I log therefore I am")
    , log = function(old, new, method,...){
        if (exists("log",envir=con)){ 
          con$log <<- "Salute"
        } else {
          con$log <<- "Hail"
        }
    }
    , initialize = function(con=new.env()){
      con <<- con
    }
  ) # end of methods
)

testfun <- function(x) x + 1

test_that("status message",{
  expect_equal(length(capture.output(logreg_status())),2)  
})


test_that("Registering and deregestering loggers",{
  expect_message(lg <- add_logger(new("envlog")))
  expect_equal(logreg_talk(),1)
  expect_equal(logreg_shutup(),0)
  expect_true(is.character(lg))
  expect_true(is.environment(get_con(lg)))
  expect_true(inherits(get_logger(lg),"envlog"))
  expect_true(rm_logger(lg))
  expect_equal(ls(LOGREG$loggers),character(0))
  expect_false(rm_logger(lg))
  expect_true(rm_logger())
  logreg_clear()
})


test_that("Registering and deregistering functions",{
  logreg_shutup()
  lg <- add_logger(new("envlog"))
  expect_true(set_log(testfun,lg))
  a <- logreg_status(quiet=TRUE)
  expect_equal(names(a$functions),"testfun")
  expect_equal(a$functions$testfun,lg)
  expect_false(unset_log(testfun,'fiets'))
  expect_true(unset_log(testfun))
  expect_false(unset_log(testfun))
})































