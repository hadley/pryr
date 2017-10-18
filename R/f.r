#' A compact syntax for anonymous functions.
#'
#' @param ... The last argument is the body of the function, all others are
#'   arguments to the function.  If there is only one argument, the formals
#'   are guessed from the code.
#' @param .env parent environment of the created function
#' @return a function
#' @export
#' @importFrom codetools findGlobals
#' @examples
#' f(x + y)
#' f(x + y)(1, 10)
#' f(x, y = 2, x + y)
#'
#' f({y <- runif(1); x + y})
f <- function(..., .env = parent.frame()) {
  dots <- match.call(expand.dots = FALSE)$`...`
  n <- length(dots)

  if (n == 1) {
    fun <- make_function(alist(... = ), dots[[1]], .env)

    names <- findGlobals(fun, merge = FALSE)$variables
    args <- stats::setNames(rep(list(substitute()), length(names)), names)
    formals(fun) <- args

    fun
  } else {
    body <- dots[[n]]
    args <- dots[-n]

    # translate unnamed args into named empty symbols
    bare <- (names(args) %||% rep("", length(args))) == ""
    bare_names <- vapply(args[bare], as.character, character(1))
    bare_names[bare_names == ".dots"] <- "..."

    args[bare] <- rep(list(substitute()), sum(bare))
    names(args)[bare] <- bare_names

    make_function(args, body, .env)
  }
}
