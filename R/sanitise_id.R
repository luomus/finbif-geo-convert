#' Sanitise input ID
#'
#' Convert FinBIF download identifier inputs of different types
#'
#' @param x A FinBIF download identifier
#'
#' @return A sanitised FinBIF download identifier
#'
#' @export

sanitise_id <- function(x) {

  ans <- list(
    file = sprintf("https://tun.fi/%s", x),
    name = x
  )

  if (!grepl("\\D+", x)) {

    ans <- list(
      file = as.integer(x),
      name = sprintf("HBF.%s", x)
    )

  }

  ans

}
