#* @apiTitle FinBIF Geographic Data Conversion API
#* @apiDescription Convert FinBIF occurrence data into geographic data formats
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiVersion 0.1.0.9000
#* @apiLicense list(name = "GPL-2.0", url = "https://opensource.org/licenses/GPL-2.0")
#* @apiTag convert Geographic conversion.

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader(
      "Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )
    res$status <- 200L
    return(list())
  } else {
    plumber::forward()
  }
}

#* Convert a FinBIF occurrence data object into geographic data format
#* @get /<input:int>/<fmt>/<geo>/<crs>
#* @param agg Aggregation. 1km or 10km. Ignored if not point data
#* @tag convert
#* @serializer contentType list(type="application/zip")
function(input, fmt, geo, crs, agg, req, res) {

  if (missing(agg)) agg <- NULL

  input <- as.integer(input)

  output_dir <- paste("HBF", input, sep = ".")

  dir.create(output_dir)
  wd <- getwd()
  on.exit(setwd(wd))
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  setwd(output_dir)

  output <- paste(output_dir, fmt, sep = ".")

  finbif_geo_convert(input, output, geo, agg, crs)

  output_zip <- paste0(output_dir, ".zip")

  zip(output_zip, list.files())
  on.exit(unlink(output_zip), add = TRUE)

  out <- readBin(output_zip, "raw", n = file.info(output_zip)$size)

  as_attachment(out, output_zip)

}
