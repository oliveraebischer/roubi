#' Reverse a string character by character
#'
#' @param x Character. Input string(s) to reverse.
#' @return Character vector of the same length with characters in reversed order.
#' @export
#' @examples
#' rob_str_reverse("abc")   # "cba"
#' rob_str_reverse(c("hello", "world"))

rob_str_reverse <- function(x) {
  stopifnot(is.character(x))
  vapply(strsplit(x, ""), \(chars) paste(rev(chars), collapse = ""), character(1L))
}
