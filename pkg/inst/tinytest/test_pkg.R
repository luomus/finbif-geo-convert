suppressWarnings(finbif_geo_convert("HBF.49381.zip", "HBF.49381.shp"))

expect_inherits(sf::st_read("HBF.49381.shp", quiet = TRUE), "sf")

unlink(
  c(
    "HBF.49381.cpg", "HBF.49381.dbf", "HBF.49381.prj", "HBF.49381.shp",
    "HBF.49381.shx"
  )
)
