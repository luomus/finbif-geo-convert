pkgs <- c(
  "callr",
  "withr",
  "httr"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

api <- callr::r_bg(
  plumber::pr_run,
  args = list(
    pr = fgc::api(file = "~/api.R"), host = "0.0.0.0", port = 8000L, quiet = TRUE
  ),
  package = "plumber"
)

suppressMessages(withr::defer(api$kill(), sys.frame()))

expect_true(api$is_alive())

Sys.sleep(3L)

r <- httr::RETRY("GET", "http://0.0.0.0", port = 8000L, path = "healthz")

expect_equal(httr::status_code(r), 200L)

withr::deferred_run(sys.frame())

expect_false(api$is_alive())
