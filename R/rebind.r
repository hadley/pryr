#' Rebind an existing name.
#'
#' This function is similar to \code{\link{<<-}} with two exceptions:
#'
#' \itemize{
#'  \item if no existing binding is found, it throws an error
#'  \item it does not recurse past the global environment into the attached
#'    packages
#'}
#'
#' @param name name of existing binding to re-assign
#' @param value new value
#' @param env environment to start search in.
#' @export
#' @examples
#' a <- 1
#' rebind("a", 2)
#' a
#' # Throws error if no existing binding
#' \dontrun{rebind("b", 2)}
#'
#' local({
#'   rebind("a", 3)
#' })
#' a
#'
#' # Can't find get because doesn't look past globalenv
#' \dontrun{rebind("get", 1)}
rebind <- function(name, value, env = parent.frame()) {
  env <- to_env(env)

  if (exists(name, env, inherits = FALSE)) {
    assign(name, value, env)
  } else {
    # Don't recurse past global or emptyenv
    if (identical(env, globalenv()) || identical(env, emptyenv())) {
      stop("Can't find ", name, call. = FALSE)
    }

    rebind(name, value, parent.env(env))
  }
}
