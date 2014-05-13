#' Make and evaluate calls.
#'
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @param .env Environment in which to evaluate call. Defaults to parent frame.
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' make_call("f", a = 1)
#' make_call(quote(f), a = 1)
#' make_call(quote(f()), a = 1)
#'
#' #' Can supply arguments individual or in a list
#' make_call(quote(f), a = 1, b = 2)
#' make_call(quote(f), list(a = 1, b = 2))
make_call <- function(f, ..., .args = list()) {
  if (is.character(f)) f <- as.name(f)
  as.call(c(f, ..., .args))
}

#' @rdname make_call
#' @export
do_call <- function(f, ..., .args = list(), .env = parent.frame()) {
  f <- substitute(f)

  call <- make_call(f, ..., .args)
  eval(call, .env)
}
