#' Find C source code for internal R functions
#'
#' Opens a link to code search on github.
#'
#' @param fun .Internal or .Primitive function call.
#' @export
#' @examples
#' \donttest{
#' show_c_source(.Internal(mean(x)))
#' show_c_source(.Primitive(sum(x)))
#' }
show_c_source  <- function(fun) {
  fun <- substitute(fun)
  stopifnot(is.call(fun))

  name <- as.character(fun[[1]])
  if (!(name %in% c(".Internal", ".Primitive"))) {
    stop("Only know how to look up .Internal and .Primitive calls",
      call. = FALSE)
  }

  internal_name <- as.character(fun[[2]][[1]])

  names <- names_c()
  found <- names[names$name == internal_name, , drop = FALSE]

  if (nrow(found) != 1) {
    stop("Could not find entry for ", internal_name, call. = FALSE)
  }

  message(internal_name, " is implemented by ", found$cfun,
    " with op = ", found$offset)

  query <- sprintf("SEXP attribute_hidden %s+repo:wch/r-source&type=Code",
    found$cfun)
  url <- paste0("https://github.com/search?q=", URLencode(query))

  if (interactive()) {
    browseURL(url)
  } else {
    message("Please visit ", url)
  }
}

#' Extract function table from names.c from R subversion repository.
#'
#' Since this is an expensive operation, it is done once and cached within
#' a session.
#'
#' @return A data frame with columns
#' \item{name}{the function name in R}
#' \item{c-entry}{The name of the corresponding C function, actually declared
#'   in ../include/Internal.h. All start with "do_", return SEXP, and
#'   have argument list (SEXP call, SEXP op, SEXP args, SEXP env)}
#' \item{offset}{the 'op' (offset pointer) above; used for C functions
#' 	 which deal with more than one R function}
#' \item{eval}{XYZ (three digits) \cr
#'  \cr
#'  X=0 says that we should force R_Visible on \cr
#'  X=1 says that we should force R_Visible off \cr
#'  X=2 says that we should switch R_Visible on but let the C code update it. \cr
#'  \cr
#'  Y=1 says that this is an internal function which must
#'      be accessed with a	.Internal(.) call, any other value is
#'      accessible directly and printed in R as ".Primitive(..)".\cr
#'  \cr
#'  Z=0 says don't evaluate (SPECIALSXP).\cr
#'  Z=1 says evaluate arguments before calling (BUILTINSXP)}
#' \item{arity}{How many arguments are required/allowed;  "-1"	meaning ``any''}
#' \item{pp-kind}{Deparsing Info (-> PPkind in ../include/Defn.h )}
#' \item{precedence}{Operator precedence (-> PPprec in ../include/Defn.h )}
#' \item{rightassoc}{Right or left associative operator}
#' @keywords internal
#' @export
names_c <- function() {
  if (exists("names_c", envir = cache)) return(cache$names_c)
  lines <- readLines("http://svn.r-project.org/R/trunk/src/main/names.c")

  # Find lines starting with {"
  fun_table <- lines[grepl("^[{][\"]", lines)]
  # Strip out {}, trailing comma and comments
  fun_table <- gsub("[{}]", "", fun_table)
  fun_table <- gsub(",$", "", fun_table)
  fun_table <- gsub("/[*].*[*]/", "", fun_table)

  table <- read.csv(text = fun_table, strip = TRUE, header = FALSE,
    stringsAsFactors = FALSE)
  names(table) <- c("name", "cfun", "offset", "eval", "arity", "pp_kind",
    "precedence", "rightassoc")

  table$eval <- sprintf("%03d", table$eval)
  table$rightassoc <- table$rightassoc == 1

  # Cache result
  cache$names_c <- table
  table
}

cache <- new.env(parent = emptyenv())
