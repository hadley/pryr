context("is_active_binding")

test_that("active bindings can be detected", {
  x <- 10
  expect_false(is_active_binding(x))

  x %<a-% runif(1)
  expect_true(is_active_binding(x))

  y <- x
  expect_false(is_active_binding(y))
})
