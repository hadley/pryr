#' Recursive ls.
#'
#' Performs \code{\link{ls}} all the way up to a top-level environment (either
#' the parent of the global environment, the empty environment or a namespace
#' environment).
#'
#' @param e environment to start the search at. Defaults to the
#'  \code{\link{parent.frame}}
#' @param all.names Show all names, even those starting with \code{.}?
#'   Defaults to \code{TRUE}, the opposite of \code{\link{ls}}
#' @export
#' @author Winston Chang
rls <- function(e = parent.frame(), all.names = TRUE) {
  if (is.function(e)) {
    e <- environment(e)
  }
  stopifnot(is.environment(e))

  if (terminal_env(e)) return()

  names <- ls(e, all.names = all.names)
  c(list(names), rls(parent.env(e), all.names = all.names))
}

terminal_env <- function(e) {
  identical(e, parent.env(globalenv())) || identical(e, emptyenv()) ||
      exists('.__NAMESPACE__.', e, inherits = FALSE)
}
