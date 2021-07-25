#' Partial apply a function, filling in some arguments.
#'
#' Partial function application allows you to modify a function by pre-filling
#' some of the arguments.  It is particularly useful in conjunction with
#' functionals and other function operators.
#'
#' @section Design choices:
#'
#' There are many ways to implement partial function application in R.
#' (see e.g. \code{dots} in \url{https://github.com/crowding/vadr} for another
#' approach.)  This implementation is based on creating functions that are as
#' similar as possible to the anonymous function that'd you'd create by hand,
#' if you weren't using \code{partial}.
#'
#' @param _f a function. For the output source to read well, this should be an
#'   be a named function.  This argument has the weird (non-syntactic) name
#'   \code{_f} so it doesn't accidentally capture any argument names begining
#'   with f.
#' @param ... named arguments to \code{f} that should be partially applied.
#' @param .env the environment of the created function. Defaults to
#'   \code{\link{parent.frame}} and you should rarely need to modify this.
#' @param .lazy If \code{TRUE} arguments evaluated lazily, if \code{FALSE},
#'   evaluated when \code{partial} is called.
#' @export
#' @examples
#' # Partial is designed to replace the use of anonymous functions for
#' # filling in function arguments. Instead of:
#' compact1 <- function(x) Filter(Negate(is.null), x)
#'
#' # we can write:
#' compact2 <- partial(Filter, Negate(is.null))
#'
#' # and the generated source code is very similar to what we made by hand
#' compact1
#' compact2
#'
#' # Note that the evaluation occurs "lazily" so that arguments will be
#' # repeatedly evaluated
#' f <- partial(runif, n = rpois(1, 5))
#' f
#' f()
#' f()
#'
#' # You can override this by saying .lazy = FALSE
#' f <- partial(runif, n = rpois(1, 5), .lazy = FALSE)
#' f
#' f()
#' f()
#'
#' # This also means that partial works fine with functions that do
#' # non-standard evaluation
#' my_long_variable <- 1:10
#' plot2 <- partial(plot, my_long_variable)
#' plot2()
#' plot2(runif(10), type = "l")
partial <- function(`_f`, ..., .env = parent.frame(), .lazy = TRUE) {
  stopifnot(is.function(`_f`))

  if (.lazy) {
    fcall <- substitute(`_f`(...))
  } else {
    fcall <- make_call(substitute(`_f`), .args = list(...))
  }
  # Pass on ... from parent function
  fcall[[length(fcall) + 1]] <- quote(...)

  args <- list("..." = quote(expr = ))
  make_function(args, fcall, .env)
}

# Alternative implementation that is much more complicated and doesn't work
# as well because missing values in the inputs to the partially applied
# function propagate and make it harder to work with.
partial2 <- function(`_f`, ..., .env = parent.frame()) {

  f_name <- substitute(`_f`)

  # Capture unevalated arguments, and convert positions to names
  dots <- match.call(expand.dots = FALSE)$`...`
  f_call <- as.call(c(list(f_name), dots))
  new_args <- as.list(match.call(`_f`, f_call))[-1]

  # Arguments to partially applied function should be the same as the original
  # function, less the arguments that have been filled in
  if (is.primitive(`_f`)) {
    # Don't know actual arguments, so fall back to ...
    arg_names <- "..."
  } else {
    arg_names <- names(formals(`_f`))
  }
  arg_names <- setdiff(arg_names, names(new_args))
  names(arg_names) <- arg_names

  reciever_args <- lapply(arg_names, function(x) quote(expr = ))
  caller_args <- c(lapply(arg_names, as.symbol), new_args)
  body <- as.call(c(f_name, caller_args))

  make_function(reciever_args, body, .env)
}
