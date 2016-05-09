context("unenclose")


expect_body_is_equal <- function(x, y) {
  expect_equal(
    deparse(body(x)),
    deparse(body(y))
  )
}

fn_a <- function(){"A"}
fn_b <- function(){"B"}
fn_c <- function(){"C"}

test_that("unencloses to parent frame", {
  my_list <- (function() {
    my_list <- list()

    for (item in c("A", "B", "C")) {
      my_fn <- function() { item }
      my_list[[item]] <- unenclose(my_fn)
    }

    my_list
  })()

  expect_body_is_equal(my_list[[1]], fn_a)
  expect_body_is_equal(my_list[[2]], fn_b)
  expect_body_is_equal(my_list[[3]], fn_c)

  expect_equal(environment(my_list[[1]]), environment())
  expect_equal(environment(my_list[[2]]), environment())
  expect_equal(environment(my_list[[3]]), environment())
})

test_that("works with lapply", {
  my_raw_list <- (function() {
    vect <- c("A", "B", "C")
    names(vect) <- vect
    lapply(vect, function(item) {
      function() {
        item
      }
    })
  })()
  my_list <- lapply(my_raw_list, unenclose)

  expect_body_is_equal(my_list[[1]], fn_a)
  expect_body_is_equal(my_list[[2]], fn_b)
  expect_body_is_equal(my_list[[3]], fn_c)

  expect_equal(environment(my_list[[1]]), parent.env(environment(my_raw_list[[1]])))
  expect_equal(environment(my_list[[2]]), parent.env(environment(my_raw_list[[2]])))
  expect_equal(environment(my_list[[3]]), parent.env(environment(my_raw_list[[3]])))
})
