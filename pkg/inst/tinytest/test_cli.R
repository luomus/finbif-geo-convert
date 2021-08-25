usage <- system2("convert", "-x", TRUE)

expect_identical(
  utils::head(usage), utils::head(system2("convert", "-h", TRUE))
)

expect_identical(
  utils::tail(usage), utils::tail(utils::capture.output(show_formats()))
)

system(
  "convert -o HBF.49381.shp HBF.49381.zip", ignore.stdout = TRUE,
  ignore.stderr = TRUE
)

expect_inherits(sf::st_read("HBF.49381.shp", quiet = TRUE), "sf")

unlink(
  c(
    "HBF.49381.cpg", "HBF.49381.dbf", "HBF.49381.prj", "HBF.49381.shp",
    "HBF.49381.shx"
  )
)
