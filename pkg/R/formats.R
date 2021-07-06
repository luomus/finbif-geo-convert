#' File formats
#'
#' Convert FinBIF data to geographic formats
#'
#' @return A named character vector of file formats.
#' @export
show_formats <- function() {
  fmts
}

fmts <- structure(
  c(
    csv = "Comma Separated Value",
    fgb = "FlatGeobuf",
    geojson = "GeoJSON",
    gml = "Geography Markup Language (GML)",
    gmt = "Generic Mapping Tools",
    gpkg = "GeoPackage",
    gxt = "Geoconcept",
    jml = "OpenJUMP JML",
    nc = "Network Common Data Format",
    ods = "Open Document/ LibreOffice / OpenOffice Spreadsheet",
    rds = "R data serialised ({sf} simple features)",
    shp = "ESRI Shapefile",
    sqlite = "SQLite / Spatialite",
    vdv = "VDV-451/VDV-452/INTREST Data Format",
    xlsx = "MS Office Open XML spreadsheet"
  ),
  class = "fmts"
)

#' @export
print.fmts <- function(x, ...) {
  cat("Extension | Description\n")
  cat("----------+----------------------------------------------------\n")
  for (i in names(x)) {
    cat(
      i, x[[i]],
      sep = paste0(paste0(rep(" ", 9 - nchar(i)), collapse = ""), " | ")
    )
    cat("\n")
  }
}
