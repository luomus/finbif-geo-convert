#' Create API
#'
#' Create the API object
#'
#' @param file Character. Path to api spec.
#' @param preroute Function. Preroute handler.
#' @param postroute Functino. Postroute handler.
#'
#' @importFrom plumber plumb pr_hooks
#'
#' @return A plumber API object.
#'
#' @export
api <- function(file, preroute = pre, postroute = post) {

  plumber::pr_hooks(
    plumber::plumb(file), list(preroute = preroute, postroute = postroute)
  )

}

#' @noRd
#' @importFrom tictoc tic
pre <- function() {

  tictoc::tic()

}

#' @noRd
#' @importFrom logger log_info log_error
#' @importFrom tictoc toc
post <- function(req, res) {

  end <- tictoc::toc(quiet = TRUE)

  log_fn <- logger::log_info

  if (res[["status"]] >= 400L) {

    log_fn <- logger::log_error

  }

  if (identical(req[["PATH_INFO"]], "/healthz")) {

    log_fn <- log_null

  }

  log_fn(
    paste0(
      '{convert_empty(req$REMOTE_ADDR)} ',
      '"{convert_empty(req$HTTP_USER_AGENT)}" ',
      '{convert_empty(req$HTTP_HOST)} ',
      '{convert_empty(req$REQUEST_METHOD)} ',
      '{convert_empty(req$PATH_INFO)} ',
      '{convert_empty(res$status)} ',
      '{round(end$toc - end$tic, digits = getOption("digits", 5L))}'
    )
  )

}


#' @noRd
log_null <- function(x) {

  NULL

}

#' @noRd
convert_empty <- function(x) {

  switch(paste0(".", x), . = "-", x)

}
