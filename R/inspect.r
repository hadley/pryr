#' Inspect internal attributes of R objects.
#'
#' \code{typename} determines the internal C typename, \code{address}
#' returns the memory location of the object, and \code{refs} returns the
#' number of references pointing to the underlying object.
#'
#' @section Non-standard evaluation:
#' All functions uses non-standard evaluation to capture the symbol you are
#' referring to and the environment in which it lives. This means that you can
#' not call any of these functions on objects created in the function call.
#' All the underlying C level functions use \code{Rf_findVar} to get to the
#' underlying SEXP.
#'
#' @param x name of object to inspect. This can not be a value.
#' @param env When inspecting environments, don't go past this one.
#' @family object inspection
#' @examples
#' x <- 1:10
#' \dontrun{.Internal(inspect(x))}
#'
#' typename(x)
#' refs(x)
#' address(x)
#'
#' y <- 1L
#' typename(y)
#' z <- list(1:10)
#' typename(z)
#' delayedAssign("a", 1 + 2)
#' typename(a)
#' a
#' typename(a)
#'
#' x <- 1:5
#' address(x)
#' x[1] <- 3L
#' address(x)
#' @name inspect
NULL

#' @export
#' @rdname inspect
inspect <- function(x, env = parent.frame()) {
  inspect_(x, env)
}

#' @export
print.inspect <- function(x, level = 0, ...) {
  indent <- paste(rep("  ", length = level), collapse = "")

  if (!x$seen) {
    cat(indent, "<", x$type, " ", x$address, ">\n", sep = "")
  } else {
    cat(indent, "[", x$type, " ", x$address, "]\n", sep = "")
  }
  if (length(x$children) > 0) {
    nms <- names(x$children) %||% rep("", length(x$children))
    Map(function(nm, val) {
      if (nm != "") cat(indent, nm, ": \n", sep = "")
      print(val, level = level + 1)
    }, nms, x$children)
  }
}

#' @export
print.inspect_NILSXP <- function(x, level = 0, ...) {
  indent <- paste(rep("  ", length = level), collapse = "")
  cat(indent, "NULL\n", sep = "")
}

#' @export
#' @rdname inspect
refs <- function(x) {
  named2(check_name(substitute(x)), parent.frame())
}

#' @export
#' @rdname inspect
address <- function(x) {
  address2(check_name(substitute(x)), parent.frame())
}


#' @export
#' @rdname inspect
typename <- function(x) {
  typename2(check_name(substitute(x)), parent.frame())
}

check_name <- function(x) {
  if (!is.name(x)) {
    stop("x must be the name of an object", call. = FALSE)
  }
  x
}

#' Track if an object is copied
#'
#' The title is somewhat misleading: rather than checking if an object is
#' modified, this really checks to see if a name points to the same object.
#'
#' @param var variable name (unquoted)
#' @param env environment name in which to track changes
#' @param quiet if \code{FALSE}, prints a message on change; if \code{FALSE}
#'   only the return value of the function is used
#' @return a zero-arg function, that when called returns a boolean indicating
#'   if the object has changed since the last time this function was called
#' @export
#' @examples
#' a <- 1:5
#' track_a <- track_copy(a)
#' track_a()
#' a[3] <- 3L
#' track_a()
#' a[3] <- 3
#' track_a()
track_copy <- function(var, env = parent.frame(), quiet = FALSE) {
  var <- substitute(var)
  force(env)

  old <- address2(var, parent.frame())
  function() {
    new <- address2(var, parent.frame())
    if (old == new) return(invisible(FALSE))

    if (!quiet) message(var, " copied")
    old <<- new
    invisible(TRUE)
  }
}
