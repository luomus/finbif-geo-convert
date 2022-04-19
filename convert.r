#!/usr/bin/env r
#
# FinBIF geo-conversion CLI
#
# Copyright (C) 2021-2022 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

library(docopt, quietly = TRUE)
library(fgc, quietly = TRUE)
library(utils, quietly = TRUE)

doc <- "Usage: convert [-h] [-x] [-v] [-o OUT] [-g GEO] [-c CRS] [-n NROWS] [-m] [FILE]

-o --output OUT          output file path. File format determined from file extension [default: output.gpkg]
-g --geometry GEO        geometry of output. One of 'point', 'bbox' or 'footprint' [default: point]
-c --crs CRS             coordinate reference system. One of 'wgs84', 'euref', 'ykj' or an EPSG code [default: wgs84]
-n --nrows NROWS         numbers of rows to extract from the input source. Defaults to all [default: -1]
-m --missing             include columns where all attributes are missing in attribute table [default: TRUE]
-h --help                show this help text
-x --usage               show help and short example usage
-v --version             show version"

opt <- docopt(doc)

if (opt$usage) {

  cat(doc, "\n\n")
  cat("where FILE is either the path to a Zip archive or tabular data file that
has been downloaded from 'laji.fi', a URI linking to such a data file or string
representing the URI (e.g., 'HBF.53254').

Examples:
  convert HBF.53254
  convert -o HBF.53254.gpkg 53254
  convert -o test.fgb HBF.53254.zip
  convert -o output1.shp HBF.53254.zip
  convert -o output2.shp -g footprint HBF.53254.zip
  convert -o output3.shp -g footprint HBF.53254

Available Output File Formats:

")
  cat(utils::capture.output(fgc::show_formats()), sep = "\n")

  q("no")

}

if (opt$version) {

  cat("fgc version ", format(packageVersion("fgc")), "\n")

  q("no")

}

if (file.exists(opt$FILE)) {

  tmp <- paste(tempdir(), opt$FILE, sep = "/")

  file.copy(opt$FILE, tmp)

  opt$FILE <- tmp

}

fgc::finbif_geo_convert(
  input  = opt$FILE,
  output = opt$output,
  geo    = opt$geometry,
  crs    = opt$crs,
  n      = as.integer(opt$nrows),
  drop_na = !opt$missing
)
