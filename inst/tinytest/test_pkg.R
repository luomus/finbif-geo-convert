library(sf, quietly = TRUE)

token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

Sys.setenv("FINBIF_ACCESS_TOKEN" = "dummy")

options(finbif_cache_path = "cache", finbif_allow_query = FALSE)

expect_inherits(finbif_geo_convert("laji-data.tsv"), "sf")

expect_inherits(finbif_geo_convert("laji-data2.tsv"), "sf")

expect_inherits(finbif_geo_convert("laji-data2.tsv", geo = "footprint"), "sf")

expect_inherits(finbif_geo_convert("HBF.6968.zip", n = 1), "sf")

finbif_geo_convert("HBF.53254.zip", "HBF.53254.gpkg", geo = "footprint")

expect_inherits(sf::st_read("HBF.53254.gpkg"), "sf")

expect_inherits(finbif_geo_convert("HBF.53254.zip", "none", "bbox"), "sf")

expect_inherits(
  finbif_geo_convert("HBF.53254.zip", "none", "bbox", crs = "euref"), "sf"
)

unlink(c("HBF.53254.gpkg", "rows_HBF.53254.tsv"))

suppressWarnings(
  finbif_geo_convert("HBF.55685.zip", "HBF.55685.gpkg", "footprint", n = 334L)
)

expect_inherits(
  sf::st_read("HBF.55685.gpkg", quiet = TRUE), "sf"
)

unlink("HBF.55685.gpkg")

mpoi <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(sf::st_point(c(0, 0)), sf::st_multipoint(cbind(0, 0)))
  )
)

expect_inherits(mpoi, "MULTIPOINT")

mpol <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(
      sf::st_polygon(
        list(
          structure(
            c(
              407684.815871683, 407684.935511214, 407685.032740082,
              407684.913100678, 407684.815871683, 7116299.29904113,
              7116303.30934417, 7116303.30644354, 7116299.2961405,
              7116299.29904113
            ),
            dim = c(5L, 2L)
          )
        )
      ),
      sf::st_polygon(
        list(
          structure(
            c(
              407732.770606645, 407751.053066859, 408304.664222537,
              408286.491510638, 407732.770606645, 7116295.30436087,
              7116908.43555739, 7116891.97707486, 7116278.84338128,
              7116295.30436087
            ),
            dim = c(5L, 2L)
          )
        )
      )
    )
  ),
  digits = 0L
)

expect_inherits(mpol, "MULTIPOLYGON")

gc <- fgc:::uncollect(
  sf::st_geometrycollection(
    list(sf::st_point(c(0, 0)), sf::st_linestring(cbind(c(0, 1), c(0, 1))))
  )
)

expect_inherits(gc, "LINESTRING")

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

Sys.setenv("FINBIF_ACCESS_TOKEN" = token)
