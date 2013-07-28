#' Return the memory address of an R object.
#' 
#' This is useful when determining if an object is modified or not. This
#' function uses non-standard evaluation to avoid incrementing the reference
#' counter for the object, which distorts investigation of in-place
#' modification.
#' 
#' @param x object to inspect
#' @export
#' @examples
#' x <- 1:5
#' address(x)
#' \dontrun{.Internal(inspect(x))}
#' x[1] <- 3L
#' address(x)
address <- function(x) {
  address2(substitute(x), parent.frame())
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