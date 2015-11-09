context("ftype")

test_that("S4 methods and generics return as expected", {
  e <- attach(NULL, name = "test")
  on.exit(detach("test"))
  
  A <- setClass("A", contains = list(), where = e)

  setGeneric("f", function(x) 1, where = e)
  f <- getGeneric("f", where = e)
  expect_equal(ftype(f), c("s4", "generic"))

  setMethod("f", signature(x = "A"), function(x) 1, where = e)
  m <- getMethod("f", signature(x = "A"), where = e)
  expect_equal(ftype(m), c("s4", "method"))    
})

test_that("RC methods return as expected", {
  B <- setRefClass("B", methods = list(f = function(x) x))
  b <- B$new()

  expect_equal(ftype(b$f), c("rc", "method"))
})

test_that("primitive_name return as expected", {

  expect_equal(primitive_name(`@`), "@")

  at <- `@`
  expect_equal(primitive_name(at), "@")
})
