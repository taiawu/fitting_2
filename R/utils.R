# library signal, import only sgolayfilt
sgolay <- function(x, p, n, m) signal::sgolayfilt(x, p = 5, n = 13, m)

# library rlang, glue
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not `{not}`.")
  }
  
  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}