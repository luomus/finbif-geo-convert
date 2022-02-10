pkgs <- c("logger", "sf")

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

log_file <- tempfile("plumber_", tempdir(), ".log")

logger::log_appender(logger::appender_tee(log_file))

expect_inherits(api(file = "~/api.R"), "Plumber")

expect_equal(fgc:::bb(NA, NA, NA, NA), list(sf::st_polygon()))

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

expect_inherits(finbif_geo_convert("laji-data.tsv"), "sf")

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

expect_inherits(
  finbif_geo_convert("HBF.53254.zip", "none", "bbox", crs = 4123), "sf"
)

expect_inherits(
  finbif_geo_convert("HBF.53254.zip", "none", "bbox", crs = "euref"), "sf"
)

expect_inherits(
  finbif_geo_convert("HBF.53254.zip", "none", "point", crs = "kkj"), "sf"
)

unlink(
  c(
    "HBF.53254_point.cpg", "HBF.53254_point.dbf", "HBF.53254_point.prj",
    "HBF.53254_point.shp", "HBF.53254_point.shx", "HBF.53254_polygon.cpg",
    "HBF.53254_polygon.dbf", "HBF.53254_polygon.prj", "HBF.53254_polygon.shp",
    "HBF.53254_polygon.shx", "HBF.53254.rds", "HBF.53254.gpkg",
    "rows_HBF.53254.tsv"
  )
)

suppressWarnings(
  finbif_geo_convert("HBF.55685.zip", "HBF.55685.shp", "footprint", n = 334L)
)

expect_inherits(
  sf::st_read("HBF.55685_multilinestring.shp", quiet = TRUE), "sf"
)

unlink(
  c(
    "HBF.55685_point.cpg", "HBF.55685_point.dbf", "HBF.55685_point.prj",
    "HBF.55685_point.shp", "HBF.55685_point.shx", "HBF.55685_polygon.cpg",
    "HBF.55685_polygon.dbf", "HBF.55685_polygon.prj", "HBF.55685_polygon.shp",
    "HBF.53254_polygon.shx", "HBF.55685_linestring.cpg",
    "HBF.55685_linestring.dbf", "HBF.55685_linestring.prj",
    "HBF.55685_linestring.shp", "HBF.53254_linestring.shx",
    "HBF.55685_multilinestring.cpg", "HBF.55685_multilinestring.dbf",
    "HBF.55685_multilinestring.prj", "HBF.55685_multilinestring.shp",
    "HBF.55685_multilinestring.shx"
  )
)

mpoi <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(sf::st_point(c(0, 0)), sf::st_multipoint(cbind(0, 0)))
  )
)

expect_inherits(mpoi, "MULTIPOINT")

mpol <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(sf::st_polygon(list(matrix(rep(0, 8), ncol = 2, byrow = TRUE))))
  )
)

expect_inherits(mpol, "MULTIPOLYGON")

gc <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(sf::st_point(c(0, 0)), sf::st_linestring(cbind(0, 0)))
  )
)

expect_inherits(gc, "GEOMETRYCOLLECTION")

tri <- fgc:::uncollect(
  sf::st_geometrycollection(sf::st_as_sfc("TRIANGLE ((0 0, 0 1, 1 0, 0 0))"))
)

expect_inherits(tri, "GEOMETRYCOLLECTION")

expect_equal(
  sanitise_id("645"),
  list(file = 645, name = "HBF.645")
)

expect_equal(
  sanitise_id("HBF.645"),
  list(file = "https://tun.fi/HBF.645", name = "HBF.645")
)

