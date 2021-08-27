pkgs <- c("logger", "sf")

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

log_file <- tempfile("plumber_", tempdir(), ".log")

logger::log_appender(logger::appender_tee(log_file))

expect_inherits(api(file = "~/api.R"), "Plumber")

expect_true(is.na(fgc:::bb(NA, NA, NA, NA)))

expect_inherits(fgc:::pre(), "numeric")

expect_null(fgc:::post(c(PATH_INFO = "/healthz"), c(status = 400)))

expect_null(
  fgc:::post(
    list(
      PATH_INFO = "", REMOTE_ADDR = "", HTTP_USER_AGENT = "", HTTP_HOST = "",
      REQUEST_METHOD = ""
    ),
    list(status = 400)
  )
)

suppressWarnings(
  finbif_geo_convert(
    "HBF.53254.zip", "HBF.53254.shp", "footprint", crs = "euref"
  )
)

expect_inherits(sf::st_read("HBF.53254_point.shp", quiet = TRUE), "sf")

expect_inherits(sf::st_read("HBF.53254_polygon.shp", quiet = TRUE), "sf")

finbif_geo_convert("HBF.53254.zip", "HBF.53254.rds")

expect_inherits(readRDS("HBF.53254.rds"), "sf")

finbif_geo_convert("HBF.53254.zip", "HBF.53254.gpkg", dwc = TRUE)

expect_inherits(sf::st_read("HBF.53254.gpkg"), "sf")

expect_inherits(finbif_geo_convert("HBF.53254.zip", "none", "bbox"), "sf")

unlink(
  c(
    "HBF.53254_point.cpg", "HBF.53254_point.dbf", "HBF.53254_point.prj",
    "HBF.53254_point.shp", "HBF.53254_point.shx", "HBF.53254_polygon.cpg",
    "HBF.53254_polygon.dbf", "HBF.53254_polygon.prj", "HBF.53254_polygon.shp",
    "HBF.53254_polygon.shx", "HBF.53254.rds", "HBF.53254.gpkg",
    "rows_HBF.53254.tsv"
  )
)
