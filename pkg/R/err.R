#' Read error message
#'
#' Read a formatted error msg.
#'
#' @param err Error to format.
#'
#' @return A list.
#'
#' @export

read_error_msg <- function(err) {

  err <- sub("^Error in .* : \n  ", "", err)

  err <- gsub("\n", "", err)

  err <- strsplit(err, "; ")

  msg <- err[[1L]][[1L]]

  msg <- sub("^Error : ", "", msg)

  name <- "error"

  if (length(err[[1L]]) > 1L) {

    name <- err[[1L]][[2L]]

    name <- sub("^err_name: ", "", name)

  }

  list(err_msg = msg, err_name = name)

}

#' @noRd

error_if <- function(condition, msg, name) {

  if (condition) {

    stop(sprintf("%s; err_name: %s", msg, name), call. = FALSE)

  }

}
