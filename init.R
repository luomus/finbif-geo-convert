library(dplyr)
library(fgc)
library(finbif)
library(future)
library(later)
library(plumber)
library(promises)
library(tools)

future::plan("multicore")

options(plumber.maxRequestSize = 1e8L)

p <- plumber::plumb("api.R")
p$run(host = "0.0.0.0", port = 8000L)
