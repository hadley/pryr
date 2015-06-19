#' Determine if a function is an S3 generic or S3 method.
#'
#' @description
#' \code{is_s3_generic} compares name checks for both internal and regular
#' generics.
#'
#' \code{is_s3_method} builds names of all possible generics for that function
#' and then checks if any of them actually is a generic.
#'
#' @param name name of function as a string. Need name of function because
#'   it's impossible to determine whether or not a function is a S3 method
#'   based only on its contents.
#' @param env environment to search in.
#' @keywords internal
#' @export
#' @examples
#' is_s3_generic("mean")
#' is_s3_generic("sum")
#' is_s3_generic("[[")
#' is_s3_generic("unlist")
#' is_s3_generic("runif")
#'
#' is_s3_method("t.data.frame")
#' is_s3_method("t.test") # Just tricking!
#' is_s3_method("as.data.frame")
#' is_s3_method("mean.Date")
is_s3_generic <- function(fname, env = parent.frame()) {
  if (!exists(fname, env)) return(FALSE)

  f <- get(fname, env, mode = "function")
  if (!is.function(f)) return(FALSE)

  if (is.primitive(f) || is_internal(f)) {
    is_internal_generic(fname)
  } else {
    uses <- findGlobals(f, merge = FALSE)$functions
    any(uses == "UseMethod")
  }
}

#' @rdname is_s3_generic
#' @export
is_s3_method <- function(name, env = parent.frame()) {
  !is.null(find_generic(name, env))
}

stop_list <- function() {
  if (getRversion() < "3.3.0") {
    getNamespace("tools")[[".make_S3_methods_stop_list"]](NULL)
  } else {
    tools::nonS3methods(NULL)
  }
}

find_generic <- function(name, env = parent.frame()) {
  if (name %in% stop_list()) return(NULL)

  pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
  n <- length(pieces)

  # No . in name, so can't be method
  if (n == 1) return(NULL)

  for(i in seq_len(n - 1)) {
    generic <- paste0(pieces[seq_len(i)], collapse = ".")
    class <- paste0(pieces[(i + 1):n], collapse = ".")
    if (is_s3_generic(generic, env)) return(c(generic, class))
  }
  NULL
}

is_internal_generic <- function(x) {
  x %in% internal_generics()
}

internal_generics <- function() {
  # Functions in S4 group generics should be the same
  group <- c(getGroupMembers("Arith"), getGroupMembers("Compare"),
    getGroupMembers("Logic"), getGroupMembers("Math"), getGroupMembers("Math2"),
    getGroupMembers("Summary"), getGroupMembers("Complex"))

  primitive <- .S3PrimitiveGenerics

  # Extracted from ?"internal generic"
  internal <- c("[", "[[", "$", "[<-", "[[<-", "$<-", "unlist",
    "cbind", "rbind", "as.vector")

  c(group, primitive, internal)
}
