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
#* @get /<input:int>/<fmt:str>/<geo:str>/<crs:str>
#* @post /<input:int>/<fmt:str>/<geo:str>/<crs:str>
#* @param agg:str Aggregation. 1km, 1km_center, 10km or 10km_center. Ignored if `geo!=point`.
#* @param select:str Which variables to select? Multiple values comma separated.
#* @param rfcts:str Record level facts. Multiple values comma separated.
#* @param efcts:str Event level facts. Multiple values comma separated.
#* @param dfcts:str Document level facts. Multiple values comma separated.
#* @param dwc:bool Use Darwin Core style column names? Ignored if `fmt==shp`.
#* @param missing:bool Keep columns containing missing data only?
#* @param timeout:dbl How long should the server be allowed to wait (in seconds) until responding (max allowed is 60)?
#* @param persist:int How long (in hours) after the request is made should the output file still be available (max 24 hours)?
#* @param file:file File to convert (maximum allowed size is ~100mb).
#* @param filetype:str One of "citable" or "lite". Only needed if `select!=all`
#* @param locale:str One of "en", "fi", or "sv". Only needed if `select!=all`
#* @tag convert
#* @serializer unboxedJSON
function(
  input, fmt, geo, crs, agg = "none", select = "all", rfcts = "none",
  efcts = "none", dfcts = "none", dwc = "false", missing = "true", timeout = 30,
  persist = 1, file = "", filetype = "citable", locale = "en", req, res
) {

  persist <- as.integer(persist)
  persist <- pmax(persist, 1L)
  persist <- pmin(persist, 24L)

  id <- paste0(as.hexmode(sample(1e9L, 1L)))

  dir.create(id)

  on.exit(later(~unlink(id, recursive = TRUE), 60L * 60L * persist))

  input_id <- input

  input <- switch(
    req$REQUEST_METHOD,
    GET = as.integer(input),
    POST = tempfile(tmpdir = id, fileext = paste0(".", file_ext(names(file))))
  )

  select <- scan(text = select, what = "char", sep = ",", quiet = TRUE)

  if (!agg %in% c("1km", "10km", "1km_center", "10km_center")) agg <- NULL

  facts <- list()

  if (!identical(rfcts, "none")) {

    facts[["record"]] <- scan(
      text = rfcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  if (!identical(efcts, "none")) {

    facts[["event"]] <- scan(
      text = efcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  if (!identical(dfcts, "none")) {

    facts[["document"]] <- scan(
      text = dfcts, what = "char", sep = ",", quiet = TRUE
    )

  }

  missing <- match.arg(missing, c("true", "false"))
  missing <- switch(missing, true = TRUE, false = FALSE)

  dwc <- match.arg(dwc, c("true", "false"))
  dwc <- switch(dwc, true = TRUE, false = FALSE)

  output <- paste0(id, "/HBF.", input_id, ".geo.", fmt)

  future_promise(
    {

      if (is.character(input)) {

        writeBin(file[[1L]], input)

      }

      finbif_geo_convert(
        input, output, geo, agg, crs, select = select, facts = facts, dwc = dwc,
        drop_na = !missing, filetype, locale
      )

      zip(
        paste0(output, ".zip"),
        setdiff(list.files(id, full.names = TRUE), input),
        flags = "-rj9X"
      )

    },
    globals = c(
      "input", "output", "geo", "agg", "crs", "select", "facts", "dwc",
      "missing", "finbif_geo_convert", "shp_write", "id", "file", "filetype",
      "locale"
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

      sleep <- .1

      timeout <- as.numeric(timeout)
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

#* @plumber
function(pr) {

  pr_set_api_spec(
    pr,
    function(spec) {

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$requestBody <- NULL

      for (i in c("filetype", "locale")) {

        pars <- spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters
        ind <- which(vapply(pars, getElement, character(1L), "name") == i)
        spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[ind]] <- NULL

      }

      spec

    }
  )

}
