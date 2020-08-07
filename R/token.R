#' Checks whether the first token is NA
#'
#' It does this by checking whether the id column in the first token is NA
#' since the id column should normally always be defined
#'
#' @param token
#'
#' @return boolean
#' @export
#'
#' @examples
token_is_na <- function(token) {
  return(is.na(token[1, "id"]))
}
