
expect_same <- function(x) {
  base <- as.vector(object.size(x))
  pryr <- as.vector(object_size(x))

  expect_equal(base, pryr)
}
