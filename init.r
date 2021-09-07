#!/usr/bin/env r
#
# FinBIF geo-conversion HTTP API
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

pkgs <- c(
  "fgc",
  "logger",
  "plumber"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

options(plumber.maxRequestSize = 1e8L)


if (identical(Sys.getenv("BRANCH"), "dev")) {

  options(finbif_dl_url = "https://staging.laji.fi/laji-etl/download")

}

log_file <- tempfile("plumber_", "logs", ".log")

logger::log_appender(logger::appender_tee(log_file))

api <- fgc::api("api.R")

plumber::pr_run(api, host = "0.0.0.0", port = 8000L, quiet = TRUE)
