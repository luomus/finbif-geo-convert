#!/usr/bin/env r
#
# FinBIF geo-conversion HTTP API
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

pkgs <- c(
  "fgc",
  "future"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

future::plan("multicore")

options(plumber.maxRequestSize = 1e8L)

fgc::run_api("logs")
