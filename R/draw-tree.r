#' Display a call (or expression) as a tree.
#'
#' @param x quoted call, list of calls, or expression to display
#' @param width displays width, defaults to current width as reported by
#'   \code{getOption("width")}
#' @param colour if \code{TRUE}, use shell escapes to colour tree.
#' @export
#' @examples
#' call_tree(quote(f(x, 1, g(), h(i()))))
#' call_tree(quote(if (TRUE) 3 else 4))
#' call_tree(expression(1, 2, 3))
#'
#' fq <- quote(f <- function(a = 1, b = 2) {a + b})
#' call_tree(fq)
#' @importFrom stringr str_c
call_tree <- function(x, width = getOption("width"), colour = interactive()) {
  if (is.expression(x) || is.list(x)) {
    trees <- vapply(x, tree, character(1), width = width, colour = colour)
    out <- str_c(trees, collapse = "\n\n")
  } else {
    out <- tree(x, width = width, colour = colour)
  }

  cat(out, "\n")
}

#' @importFrom stringr str_c str_length str_sub
str_trunc <- function(x, width = getOption("width")) {
  ifelse(str_length(x) <= width, x, str_c(str_sub(x, 1, width - 3), "..."))
}

is.constant <- function(x) !is.language(x)
is.leaf <- function(x) {
  is.constant(x) || is.name(x) || (is.call(x) && length(x) == 1)
}

#' @importFrom stringr str_c str_dup
tree <- function(x, level = 1, width = getOption("width"), colour = FALSE) {
  indent <- str_c(str_dup("  ", level - 1), "\\- ")
  label <- label(x, width - nchar(indent), colour = colour)

  if (is.leaf(x)) return(str_c(indent, label))

  children <- vapply(as.list(x[-1]), tree, character(1),
    level = level + 1, width = width, colour = colour)

  str_c(indent, label, "\n", str_c(children, collapse = "\n"))
}

#' @importFrom testthat colourise
#' @importFrom stringr str_c
label <- function(x, width = getOption("width"), colour = FALSE) {
  if (is.call(x)) {
    label <- str_c(as.character(x[[1]]), "()")
    col <- "red"
  } else if (is.name(x)) {
    label <- str_c("`", as.character(x))
    col <- "blue"
  } else {
    label <- deparse(x)[[1]]
    col <- "black"
  }

  lbl <- str_trunc(label, width)
  if (colour) colourise(lbl, col) else lbl
}
