context("Method from call")

e <- new.env()
setClass("A", "list", where = e)
setClass("B", "list", where = e)
setGeneric("gen0", function(x, ...) standardGeneric("gen0"), where = e)

test_that("finds method with missing args", {
  setMethod("gen0", "missing", function(x, ...) "missing", where = e)

  exp <- selectMethod("gen0", "missing")
  expect_identical(method_from_call(gen0(), e), exp)
})

test_that("only uses arguments in generic", {
  setMethod("gen0", "A", function(x, ...) "A", where = e)

  exp <- selectMethod("gen0", "A")
  expect_identical(method_from_call(gen0(new("A"), 1), e), exp)
})
