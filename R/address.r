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
#' @export
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