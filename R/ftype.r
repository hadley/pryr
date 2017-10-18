#' Determine function type.
#' 
#' This function figures out whether the input function is a 
#' regular/primitive/internal function, a internal/S3/S4 generic, or a 
#' S3/S4/RC method. This is function is slightly simplified as it's possible
#' for a method from one class to be a generic for another class, but that
#' seems like such a bad idea that hopefully no one has done it.
#' 
#' @param f unquoted function name
#' @return a character of vector of length 1 or 2.
#' @family object inspection
#' @importFrom methods is
#' @export
#' @examples
#' ftype(`%in%`)
#' ftype(sum)
#' ftype(t.data.frame)
#' ftype(t.test) # Tricky!
#' ftype(writeLines)
#' ftype(unlist)
ftype <- function(f) { 
  fexpr <- substitute(f)
  env <- parent.frame()
  fname <- if (is.name(fexpr)) as.character(fexpr) else NULL
  
  if (is.primitive(f)) {
    c("primitive", if (is_internal_generic(primitive_name(f))) "generic")
  } else if (is_internal(f)) {
    c("internal", if (is_internal_generic(internal_name(f))) "generic")
  } else if (is(f, "standardGeneric")) {
    c("s4", "generic")
  } else if (is(f, "MethodDefinition")) {
    c("s4", "method")
  } else if (is(f, "refMethodDef")) {
    c("rc", "method")
  } else if (!is.null(fname) && is_s3_generic(fname, env)) {
    c("s3", "generic")
  } else if (!is.null(fname) && is_s3_method(fname, env)) {
    c("s3", "method")
  } else {
    c("function")
  }  
}

# Hacky method to get name of primitive function
primitive_name <- function(f) {
  stopifnot(is.primitive(f))

  str <- deparse(f)
  match <- regexec(".Primitive\\([\"](.*?)[\"]\\)", str)
  regmatches(str, match)[[1]][2]
}

is_internal <- function(f) {
  if (!is.function(f) || is.primitive(f)) return(FALSE)
  calls <- findGlobals(f, merge = FALSE)$functions
  any(calls %in% ".Internal")
}

# fs <- stats::setNames(lapply(ls("package:base"), get), ls("package:base"))
# internal <- Filter(is_internal, fs)
# icall <- sapply(internal, internal_name)
# icall[names(icall) != icall]
internal_name <- function(f) {
  
  internal_call <- function(x) {
    if (is.name(x) || is.atomic(x)) return(NULL)
    if (identical(x[[1]], quote(.Internal))) return(x)
  
    # Work backwards since likely to be near end last 
    # (and e.g. unlist has multiple .Internal calls)
    for (i in rev(seq_along(x))) {
      icall <- internal_call(x[[i]])
      if (!is.null(icall)) return(icall)
    }
    NULL
  }
  call <- internal_call(body(f))
  as.character(call[[2]][[1]])
}
