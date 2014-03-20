context("bytes")

test_that("bytes produces hex representations as expected", {

  expect_identical(
    bytes(1L),
    c("00 00 00 01")
  )

  expect_identical(
    bytes(1),
    "3F F0 00 00 00 00 00 00"
  )

  expect_identical(
    bytes("aa"),
    paste(bytes("a"), bytes("a"))
  )

})

test_that("bytes produces binary representations as expected", {

  expect_identical(
    bits(1L),
    "00000000 00000000 00000000 00000001"
  )

  expect_identical(
    bits(1),
    "00111111 11110000 00000000 00000000 00000000 00000000 00000000 00000000"
  )

})

test_that("encoding doesn't affect what bits / bytes are read", {

  x <- y <- z <- "\u9b3c"
  Encoding(y) <- "bytes"
  Encoding(z) <- "latin1"
  expect_identical( bytes(x), bytes(y) )
  expect_identical( bytes(y), bytes(z) )

  expect_identical( bits(x), bits(y) )
  expect_identical( bits(y), bits(z) )

})
