#!/usr/bin/env r
#
# FinBIF geo-conversion HTTP API
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

pkgs <- c(
  "digest",
  "future",
  "fgc",
  "later",
  "logger",
  "plumber",
  "promises",
  "rapidoc",
  "tools"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

future::plan("multicore", workers = 20L)

options(plumber.maxRequestSize = 1e8L)

if (identical(Sys.getenv("BRANCH"), "dev")) {

  options(finbif_dl_url = "https://staging.laji.fi/laji-etl/download")

  assignInNamespace("var_names", finbif:::var_names_test, "finbif")
  assignInNamespace("filter_names", finbif:::filter_names_test, "finbif")
  assignInNamespace("has_value", finbif:::has_value_test, "finbif")
  assignInNamespace(
    "lite_download_file_vars", finbif:::lite_download_file_vars_test, "finbif"
  )
  assignInNamespace("cite_file_vars", finbif:::cite_file_vars_test, "finbif")

}

log_file <- tempfile("plumber_", "logs", ".log")

logger::log_appender(logger::appender_tee(log_file))

if (!dir.exists("logs/errors")) dir.create("logs/errors", recursive = TRUE)

api <- fgc::api("api.R")

plumber::pr_run(api, host = "0.0.0.0", port = 8000L)
