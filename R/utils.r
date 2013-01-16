all_named <- function(x) {
  if (length(x) == 0) return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}

"%||%" <- function(x, y) if (is.null(x)) y else x
