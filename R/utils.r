all_named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}
