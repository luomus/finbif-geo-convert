usage <- system2("convert", "-x", TRUE)

expect_identical(
  utils::head(usage), utils::head(system2("convert", "-h", TRUE))
)

expect_identical(
  utils::tail(usage), utils::tail(utils::capture.output(show_formats()))
)

system(
  "convert -o HBF.53254.shp HBF.53254.zip", ignore.stdout = TRUE,
  ignore.stderr = TRUE
)

expect_inherits(sf::st_read("HBF.53254_point.shp", quiet = TRUE), "sf")

unlink(
  c(
    "HBF.53254_point.cpg", "HBF.53254_point.dbf", "HBF.53254_point.prj",
    "HBF.53254_point.shp", "HBF.53254_point.shx"
  )
)
