#!/usr/bin/env r
#
# FinBIF geo-conversion HTTP API
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

pkgs <- c(
  "fgc",
  "future",
  "logger",
  "plumber"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

future::plan("multicore")

options(plumber.maxRequestSize = 1e8L)

log_file <- tempfile("plumber_", "logs", ".log")

logger::log_appender(logger::appender_tee(log_file))

api <- fgc::api("api.R")

plumber::pr_run(api, host = "0.0.0.0", port = 8000L, quiet = TRUE)
