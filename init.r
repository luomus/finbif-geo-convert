#!/usr/bin/env r
#
# FinBIF geo-conversion HTTP API
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

pkgs <- c(
  "digest",
  "dplyr",
  "fgc",
  "finbif",
  "future",
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

future::plan("multicore")

options(plumber.maxRequestSize = 1e8L)

log_dir <- "logs"
dir.create(log_dir)
log_file <- tempfile("plumber_", log_dir, ".log")
logger::log_appender(
  logger::appender_tee(log_file)
)

convert_empty <- \(x) switch(paste0(".", x), . = "-", x)

p <- plumber::plumb("api.R")

p$registerHooks(
  list(
    preroute = function() tictoc::tic(),
    postroute = function(req, res) {
      end <- tictoc::toc(quiet = TRUE)
      logger::log_info(
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
  )
)

p$run(host = "0.0.0.0", port = 8000L, quiet = TRUE)