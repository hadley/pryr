#' Find all functions in that call supplied functions.
#'
#' @param envs Vector of environments to look in. Can be specified by
#'   name, position or as environment
#' @param funs Functions to look for
#' @param match_any If \code{TRUE} return functions that use any of \code{funs}.
#'   If \code{FALSE}, return functions that use all of \code{funs}.
#' @export
#' @examples
#' names(find_uses("package:base", "sum"))
#'
#' envs <- c("package:base", "package:utils", "package:stats")
#' funs <- c("match.call", "sys.call")
#' find_uses(envs, funs)
find_uses <- function(envs, funs, match_any = TRUE) {
  envs <- lapply(envs, to_env, quiet = TRUE)

  by_env <- lapply(envs, function(env) {
    names <- ls(envir = env)
    names(names) <- names
    compact(lapply(names, function(x) matched_calls(get(x, envir = env), funs,
      match_any = match_any)))
  })

  unlist(by_env, recursive = FALSE)
}

matched_calls <- function(fun, calls, match_any = TRUE) {
  if (!is.function(fun) || is.primitive(fun)) return()

  called <- fun_calls(fun)
  matches <- calls %in% called
  match <- if (match_any) any(matches) else all(matches)

  if (!match) return()
  called[matches]
}
