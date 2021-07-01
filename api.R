#* @apiTitle FinBIF Geographic Data Conversion API
#* @apiDescription Convert FinBIF occurrence data into geographic data formats
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiVersion 0.1.0.9002
#* @apiLicense list(name = "GPL-2.0", url = "https://opensource.org/licenses/GPL-2.0")
#* @apiTag convert Geographic conversion.

#* @filter cors
cors <- function(req, res) {

  res$setHeader("Access-Control-Allow-Origin", "*")

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {

    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader(
      "Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )

    res$status <- 200L

    list()

  } else {

    forward()

  }

}

#* Convert a FinBIF occurrence data object into a geographic data format
#* @get /<input:int>/<fmt>/<geo>/<crs>
#* @param agg:str Aggregation. 1km or 10km. Ignored if `geo!=point`.
#* @param select:str Which variables to select? Multiple values comma separated.
#* @param rfcts:str Record level facts. Multiple values comma separated.
#* @param efcts:str Event level facts. Multiple values comma separated.
#* @param dfcts:str Document level facts. Multiple values comma separated.
#* @param dwc:bool Use Darwin Core style column names? Ignored if `fmt==shp`.
#* @param missing:bool Keep columns containing missing data only?
#* @param timeout:dbl How long should the server be allowed to wait (in seconds) until responding (max allowed is 60).
#* @tag convert
#* @serializer unboxedJSON
function(
  input, fmt, geo, crs, agg, select, rfcts, efcts, dfcts, dwc = "false",
  missing = "true", timeout = 30, res
) {

  if (missing(select)) {

    select <- "all"

  } else {

    select <- scan(text = select, what = "char", sep = ",", quiet = TRUE)

  }

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

  missing <- match.arg(missing, c("true", "false"))
  missing <- switch(missing, true = TRUE, false = FALSE)

  dwc <- match.arg(dwc, c("true", "false"))
  dwc <- switch(dwc, true = TRUE, false = FALSE)

  input <- as.integer(input)

  id <- paste0(as.hexmode(sample(1e9L, 1L)))
  dir.create(id)
  on.exit(later(~unlink(id, recursive = TRUE), 60L * 60L))

  output <- paste0(id, "/HBF.", input, ".geo.", fmt)

  future_promise(
    {

      finbif_geo_convert(
        input, output, geo, agg, crs, select = select, facts = facts, dwc = dwc,
        drop_na = !missing
      )

      zip(
        paste0(output, ".zip"),
        list.files(id, full.names = TRUE),
        flags = "-rj9X"
      )

    },
    globals = c(
      "input", "output", "geo", "agg", "crs", "select", "facts", "dwc",
      "missing", "finbif_geo_convert", "bb", "fmts", "short_geo_col_nms",
      "geo_components", "shp_write", "id"
    ),
    packages = c("dplyr", "finbif", "sf", "stats", "stringi", "tools")
  )

  res$status <- 303L
  res$setHeader("Location", paste0("/status/", id, "?timeout=", timeout))

  id

}

#* @get /status/<id:str>
#* @param timeout:dbl How long should the server be allowed to wait (in seconds) until responding (max allowed is 60).
#* @tag convert
#* @serializer unboxedJSON
function(id, timeout = 30L, res) {

  if (!dir.exists(id)) {

    res$status <- 404L
    return("File not found")

  }

  poll <- future_promise(
    {

      zip <- character()

      sleep <- .2
      timeout <- pmax(timeout, sleep)
      timeout <- pmin(timeout, 60)

      timer <- 0

      while (length(zip) < 1L) {

        zip <- list.files(id, pattern = "^HBF.*\\.zip$")

        timer <- timer + sleep

        if (timer > timeout) {

          zip <- "pending"

        }

        Sys.sleep(sleep)

      }

      zip

    },
    globals = c("id", "timeout")
  )

  then(
    poll,
    ~{

      if (!identical(., "pending")) {

        res$status <- 303L
        res$setHeader("Location", paste0("/output/", id))

      }

      .

    }
  )

}

#* @get /output/<id:str>
#* @tag convert
#* @serializer contentType list(type="application/zip")
function(id, res) {

  if (!dir.exists(id)) {

    res$serializer <- serializer_unboxed_json()
    res$status <- 404L
    return("File not found")

  }

  zip <- list.files(id, pattern = "^HBF.*\\.zip$", full.names = TRUE)

  out <- readBin(zip, "raw", n = file.info(zip)$size)

  as_attachment(out, zip)

}
