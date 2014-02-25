#' Display a call (or expression) as a tree.
#'
#' \code{call_tree} takes a quoted expression. \code{ast} does the quoting
#' for you.
#'
#' @param x quoted call, list of calls, or expression to display
#' @param width displays width, defaults to current width as reported by
#'   \code{getOption("width")}
#' @export
#' @examples
#' call_tree(quote(f(x, 1, g(), h(i()))))
#' call_tree(quote(if (TRUE) 3 else 4))
#' call_tree(expression(1, 2, 3))
#'
#' ast(f(x, 1, g(), h(i())))
#' ast(if (TRUE) 3 else 4)
#' ast(function(a = 1, b = 2) {a + b})
#' ast(f()()())
#' @importFrom stringr str_c
call_tree <- function(x, width = getOption("width")) {
  if (is.expression(x) || is.list(x)) {
    trees <- vapply(x, tree, character(1), width = width)
    out <- str_c(trees, collapse = "\n\n")
  } else {
    out <- tree(x, width = width)
  }

  cat(out, "\n")
}

#' @rdname call_tree
#' @export
ast <- function(x) call_tree(substitute(x))

#' @importFrom stringr str_c str_length str_sub
str_trunc <- function(x, width = getOption("width")) {
  ifelse(str_length(x) <= width, x, str_c(str_sub(x, 1, width - 3), "..."))
}

#' @importFrom stringr str_c str_dup
tree <- function(x, level = 1, width = getOption("width"), branch = "\\- ") {
  indent <- str_c(str_dup("  ", level - 1), branch)

  if (is.atomic(x) && length(x) == 1) {
    label <- paste0(" ", deparse(x)[1])
    children <- NULL
  } else if (is.name(x)) {
    x <- as.character(x)
    if (x == "") {
      # Special case the missing argument
      label <- "`MISSING"
    } else {
      label <- paste0("`", as.character(x))
    }

    children <- NULL
  } else if (is.call(x)) {
    label <- "()"
    children <-  vapply(as.list(x), tree, character(1),
      level = level + 1, width = width - 3)
  } else if (is.pairlist(x)) {
    label <- "[]"

    branches <- paste("\\", format(names(x)), "=")
    children <- character(length(x))
    for (i in seq_along(x)) {
      children[i] <- tree(x[[i]], level = level + 1, width = width - 3,
        branch = branches[i])
    }
  } else {
    # Special case for srcrefs, since they're commonly seen
    if (inherits(x, "srcref")) {
      label <- "<srcref>"
    } else {
      label <- paste0("<", typeof(x), ">")
    }
    children <- NULL
  }

  label <- str_trunc(label, width - 3)

  if (is.null(children)) {
    paste0(indent, label)
  } else {
    paste0(indent, label, "\n", paste0(children, collapse = "\n"))
  }
}
