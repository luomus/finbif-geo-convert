#!/usr/bin/env r
#
# FinBIF geo-conversion CLI
#
# Copyright (C) 2021 LUOMUS - Finnish Museum of Natural History
#
# Released under GPL (>= 2)

library(docopt, quietly = TRUE)
library(fgc, quietly = TRUE)
library(utils, quietly = TRUE)

doc <- "Usage: convert [-h] [-x] [-o OUT] [-g GEO] [-a AGG] [-c CRS] [-s SLCT...] [-n NROWS] [-r RFCT...] [-e EFCT...] [-d DFCT...] [-t TYPE] [-l LC] [-w] [-m] [FILE]

-o --output OUT          output file path. File format determined from file extension [default: output.gpkg]
-g --geometry GEO        geometry of output. One of 'point', 'bbox' or 'footprint' [default: point]
-a --aggregation AGG     aggregate features to a KKJ based grid. One of '1km', '10km', '1km_center' or '10km_center' [default: none]
-c --crs CRS             coordinate reference system. One of 'wgs84', 'euref', 'kkj' or an EPSG code [default: wgs84]
-s --select SLCT         variables to include in the attribute table [default: all]
-n --nrows NROWS         numbers of rows to extract from the input source. Defaults to all [default: -1]
-r --record-facts RFCT   record level facts to extract from the input source [default: none]
-e --event-facts EFCT    event level facts to extract from the input source [default: none]
-d --document-facts DFCT document level facts to extract from the input source [default: none]
-t --type TYPE           file type of the input source. One of 'citable' or 'lite' [default: citable]
-l --locale LC           locale of the input source. One of 'en', 'fi' or 'sv' [default: en]
-w --darwin              use Darwin Core style variable names [default: FALSE]
-m --missing             include columns where all attributes are missing in attribute table [default: TRUE]
-h --help                show this help text
-x --usage               show help and short example usage"

opt <- docopt(doc)

if (opt$usage) {

  cat(doc, "\n\n")
  cat("where FILE is either the path to a Zip archive or tabular data file that
has been downloaded from 'laji.fi', a URI linking to such a data file or an
integer representing the URI (e.g., '49381').

Examples:
  convert 53254
  convert -o HBF.49381.gpkg 49381
  convert -o test.fgb HBF.49381.zip
  convert -o output1.shp HBF.49381.zip
  convert -o output2.shp -g footprint HBF.49381.zip
  convert -o output3.shp -g footprint 53254

Available Output File Formats:

")
  cat(utils::capture.output(fgc::show_formats()), sep = "\n")

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
  agg    = switch(opt$aggregation, none = NULL, opt$aggregation),
  crs    = opt$crs,
  select = opt$select,
  n      = as.integer(opt$nrows),
  facts  = list(
    record   = switch(opt$record_facts, none = NULL, opt$record_facts),
    event    = switch(opt$event_facts, none = NULL, opt$event_facts),
    document = switch(opt$document_facts, none = NULL, opt$document_facts)
  ),
  filetype = opt$type,
  locale = opt$locale,
  dwc = opt$darwin,
  drop_na = !opt$missing
)
