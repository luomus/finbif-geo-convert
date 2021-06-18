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
#* @param rfcts Record level facts. Multiple values comma separated.
#* @param efcts Event level facts. Multiple values comma separated.
#* @param dfcts Document level facts. Multiple values comma separated.
#* @tag convert
#* @serializer contentType list(type="application/zip")
function(input, fmt, geo, crs, agg, rfcts, efcts, dfcts, req, res) {

  if (missing(agg) || !agg %in% c("1km", "10km")) agg <- NULL

  facts <- list()

  if (!missing(rfcts)) {

    facts[["record"]] <- scan(
      text = rfcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  if (!missing(efcts)) {

    facts[["event"]] <- scan(
      text = efcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  if (!missing(dfcts)) {

    facts[["document"]] <- scan(
      text = dfcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  input <- as.integer(input)

  pwd <- getwd()
  on.exit(setwd(pwd))

  work_dir <- paste0("wd", as.hexmode(sample(1e9L, 1L)))
  dir.create(work_dir)
  setwd(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  output_dir <- paste("HBF", input, "geo", sep = ".")

  dir.create(output_dir)

  setwd(output_dir)

  output <- paste(output_dir, fmt, sep = ".")

  finbif_geo_convert(input, output, geo, agg, crs, facts = facts)

  output_zip <- paste0(output_dir, ".zip")

  zip(output_zip, list.files())

  out <- readBin(output_zip, "raw", n = file.info(output_zip)$size)

  as_attachment(out, output_zip)

}
