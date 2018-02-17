context("track_copy")

test_that("deletes are not copies", {
  a <- 1:5
  tracker <- track_copy(a, quiet = TRUE)

  expect_false(tracker())
  rm(a)
  expect_false(tracker())
})

test_that("modifying type triggers copy", {
  a <- 1:5
  tracker <- track_copy(a, quiet = TRUE)

  expect_false(tracker())
  a[3] <- 2.5
  expect_true(tracker())
})

test_that("modifying element in vector does not trigger copy", {
  a <- c(1L, 2L, 5L, 4L, 3L)
  tracker <- track_copy(a, quiet = TRUE)

  expect_false(tracker())
  a[3] <- 3L
  expect_false(tracker())
})
