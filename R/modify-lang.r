#' Recursively modify a language object
#'
#' @param x object to modify: should be a call, expression, function or
#'   list of the above.
#' @param f function to apply to leaves
#' @param ... other arguments passed to \code{f}
#' @export
#' @examples
#' a_to_b <- function(x) {
#'   if (is.name(x) && identical(x, quote(a))) return(quote(b))
#'   x
#' }
#' examples <- list(
#'   quote(a <- 5),
#'   alist(a = 1, c = a),
#'   function(a = 1) a * 10,
#'   expression(a <- 1, a, f(a), f(a = a))
#' )
#' modify_lang(examples, a_to_b)
#' # Modifies all objects called a, but doesn't modify arguments named a
modify_lang <- function(x, f, ...) {
  recurse <- function(y) {
    # if (!is.null(names(y))) names(y) <- f2(names(y))
    lapply(y, modify_lang, f = f, ...)
  }

  if (is.atomic(x) || is.name(x)) {
    # Leaf
    f(x, ...)
  } else if (is.call(x)) {
    as.call(recurse(x))
  } else if (is.function(x)) {
    formals(x) <- modify_lang(formals(x), f, ...)
    body(x) <- modify_lang(body(x), f, ...)
    x
  } else if (is.pairlist(x)) {
    # Formal argument lists (when creating functions)
    as.pairlist(recurse(x))
  } else if (is.expression(x)) {
    # shouldn't occur inside tree, but might be useful top-level
    as.expression(recurse(x))
  } else if (is.list(x)) {
    # shouldn't occur inside tree, but might be useful top-level
    recurse(x)
  } else {
    stop("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
  }
}

