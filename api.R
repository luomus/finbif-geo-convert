#* @apiTitle FinBIF Geographic Data Conversion API
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiLicense list(name = "GPL-2.0", url = "https://opensource.org/licenses/GPL-2.0")
#* @apiTag formats Output file formats
#* @apiTag convert Convert a FinBIF occurrence data file into a geographic data format
#* @apiTag status Check status of API

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

    plumber::forward()

  }

}

#* Check the liveness of the API
#* @get /healthz
#* @head /healthz
#* @tag status
#* @response 200 A json object
#* @serializer unboxedJSON
function() {
 ""
}

#* Get a list of output file formats
#* @get /formats
#* @tag formats
#* @response 200 A json object
#* @serializer unboxedJSON
function() {
  fmts <- show_formats()
  list(formats = data.frame(name = names(fmts), description = unname(c(fmts))))

}

#* Convert FinBIF data file with persistent identifier
#* @get /<input:int>/<fmt:str>/<geo:str>/<crs:str>
#* @post /<input:int>/<fmt:str>/<geo:str>/<crs:str>
#* @param input:int The integer representation of the input file's identifier.
#* @param fmt:str The output file format (in the form of a file extension) for the geographic data.
#* @param geo:str The geometry type of the output. One of 'point', 'bbox' or 'footprint'.
#* @param crs:str The coordinate reference system for the output. One of "kkj", "euref", "wgs84" or any valid numeric EPSG code.
#* @param agg:str Aggregation. 1km, 1km_center, 10km or 10km_center. Ignored if `geo != point`.
#* @param select:str Which variables to select? Multiple values comma separated.
#* @param rfcts:str Record level facts. Multiple values comma separated.
#* @param efcts:str Event level facts. Multiple values comma separated.
#* @param dfcts:str Document level facts. Multiple values comma separated.
#* @param dwc:bool Use Darwin Core style column names? Ignored if `fmt == shp`.
#* @param missing:bool Keep columns containing missing data only?
#* @param timeout:dbl How long should the server be allowed to wait (in seconds) until responding (max allowed is 60)?
#* @param persist:int How long (in hours) after the request is made should the output file still be available (max 24 hours)?
#* @param file:file File to convert (maximum allowed size is ~100mb).
#* @param filetype:str One of "citable" or "lite". Only needed if `select != all`
#* @param locale:str One of "en", "fi", or "sv". Only needed if `select != all`
#* @tag convert
#* @response 303 A json object
#* @serializer unboxedJSON
function(
  input, fmt, geo, crs, agg = "none", select = "all", rfcts = "none",
  efcts = "none", dfcts = "none", dwc = "false", missing = "true", timeout = 30,
  persist = 1, file = "", filetype = "citable", locale = "en", req, res
) {

  persist <- as.integer(persist)
  persist <- pmax(persist, 1L)
  persist <- pmin(persist, 24L)

  id <- digest::digest(list(req, sample(1e9L, 1L)))

  dir.create(id)

  on.exit(later::later(~unlink(id, recursive = TRUE), 60L * 60L * persist))

  input_id <- input

  input <- switch(
    req$REQUEST_METHOD,
    GET = as.integer(input),
    POST = tempfile(
      tmpdir = id, fileext = paste0(".", tools::file_ext(names(file)))
    )
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

  promises::future_promise(
    {

      if (is.character(input)) {

        writeBin(file[[1L]], input)

      }

      res <- try(
        fgc::finbif_geo_convert(
          input, output, geo, agg, crs, select = select, facts = facts,
          filetype = filetype, locale = locale, dwc = dwc, drop_na = !missing,
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {

         writeLines(res[[1L]], paste0(id, "/error.txt"))

      } else {

        zip(
          paste0(output, ".zip"),
          setdiff(list.files(id, full.names = TRUE), input),
          flags = "-rj9qX"
        )

      }

    },
    globals = c(
      "input", "output", "geo", "agg", "crs", "select", "facts", "dwc",
      "missing", "id", "file", "filetype", "locale"
    ),
    packages = "fgc"
  )

  res$status <- 303L
  res$setHeader("Location", paste0("/status/", id, "?timeout=", timeout))

  c(id = id)

}

#* Get status of conversion
#* @get /status/<id:str>
#* @param id:str The identifier of a conversion.
#* @param timeout:dbl How long should the server be allowed to wait (in seconds) until responding (max allowed is 60).
#* @tag convert
#* @response 200 A json object
#* @response 303 A json object
#* @response 400 Client error
#* @response 404 File not found
#* @serializer unboxedJSON
function(id, timeout = 30L, res) {

  if (!dir.exists(id)) {

    res$status <- 404L
    return("File not found")

  }

  poll <- promises::future_promise(
    {

      status <- character()

      sleep <- .1

      timeout <- as.numeric(timeout)
      timeout <- pmax(timeout, sleep)
      timeout <- pmin(timeout, 60)

      timer <- 0

      while (length(status) < 1L) {

        status <- list.files(id, pattern = "^HBF.*\\.zip$")

        timer <- timer + sleep

        if (timer > timeout) {

          status <- "pending"

        }

        err <- paste0(id, "/error.txt")

        if (file.exists(err)) {

          status <- "error"

        }

        Sys.sleep(sleep)

      }

      status

    },
    globals = c("id", "timeout")
  )

  promises::then(
    poll,
    ~{

      if (identical(., "pending")) {

         list(id = id, status = .)

      } else if (identical(., "error")) {

        res$status <- 400L

        error <- paste0(id, "/error.txt")

        list(id = id, error = readChar(error, file.info(error)$size))

      } else {

        res$status <- 303L
        res$setHeader("Location", paste0("/output/", id))

        list(id = id, status = "complete")

      }

    }
  )

}
#* Get the output file of conversion
#* @get /output/<id:str>
#* @param id:str The identifier of a conversion.
#* @tag convert
#* @response 200 A ZIP archive file attachment
#* @response 404 File not found
#* @serializer contentType list(type="application/zip")
function(id, res) {

  if (!dir.exists(id)) {

    res$serializer <- plumber::serializer_unboxed_json()
    res$status <- 404L
    return("File not found")

  }

  zip <- list.files(id, pattern = "^HBF.*\\.zip$", full.names = TRUE)

  out <- readBin(zip, "raw", n = file.info(zip)$size)

  plumber::as_attachment(out, zip)

}

#* @assets /usr/local/lib/R/site-library/finbif/help/figures
list()

#* @assets ./static /
list()

#* @plumber
function(pr) {

  version <- as.character(utils::packageVersion("fgc"))

  plumber::pr_set_api_spec(
    pr,
    function(spec) {

      spec$info$version <- version

      spec$info$description <- readChar("api.md", file.info("api.md")$size)

      spec$paths$`/formats`$get$description <- paste0(
        "Get a list of geographic data file formats that FinBIF occurrence can ",
        "be converted to as a JSON object."
      )
      spec$paths$`/formats`$get$responses$`500`$content <- NULL
      spec$paths$`/formats`$get$responses$default <- NULL
      spec$paths$`/formats`$get$responses$`200`$content$`application/json`$schema <- list(
        type = "object",
        properties = list(
          formats = list(
            type = "array",
            items = list(
              type = "object",
              required = c("name", "description"),
              properties = list(
                name = list(
                  type = "string",
                  description = "File extension."
                ),
                description = list(
                  type = "string",
                  description = "File format description."
                )
              )
            )
          )
        )
      )

      spec$paths$`/formats`$get$responses$`200`$content$`application/json`$example <- list(
        formats = data.frame(
          name = c("ext1", "ext2"),
          description = c(
            "File format description 1", "File format description 2"
          )
        )
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$summary <-
        "Convert FinBIF data file via data upload"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$description <- paste0(
        "Convert a FinBIF citable or lite data file to a geographic data ",
        "format via upload."
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[1]]$description <- paste0(
        "An integer identifier for the geographic data file. ",
        "Will be used in the filename of the download."
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[1]]$example <-
        1234

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[2]]$example <-
        "gpkg"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[3]]$example <-
        "point"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[4]]$example <-
        "wgs84"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$responses$`500`$content <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$responses$default <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$responses$`200` <- NULL

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$responses$`303`$content$`application/json`$schema <- list(
        type = "object",
        required = "id",
        properties = list(
          id = list(
            type = "string",
            description = "Identifier of the file conversion.",
            example = "17ecf252"
          )
        )
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[1]]$example <-
        53254

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[2]]$example <-
        "gpkg"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[3]]$example <-
        "point"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[4]]$example <-
        "wgs84"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$requestBody <- NULL

      for (i in c("filetype", "locale")) {

        pars <- spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters
        ind <- which(vapply(pars, getElement, character(1L), "name") == i)
        spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[ind]] <- NULL

      }

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$description <- paste0(
        "Convert a FinBIF citable data download file to a geographic data ",
        "format via its persistent identifier."
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$`303`$content$`application/json`$schema <- list(
        type = "object",
        required = "id",
        properties = list(
          id = list(
            type = "string",
            description = "Identifier of the file conversion.",
            example = "17ecf252"
          )
        )
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$`500`$content <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$default <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$`200` <- NULL

      spec$paths$`/status/{id}`$get$description <-
        "Get the status of a conversion using an identifier."

      spec$paths$`/status/{id}`$get$parameters[[1]]$example <- "17ecf252"

      spec$paths$`/status/{id}`$get$responses$`500`$content <- NULL
      spec$paths$`/status/{id}`$get$responses$`404`$content <- NULL
      spec$paths$`/status/{id}`$get$responses$`400`$content <- NULL
      spec$paths$`/status/{id}`$get$responses$default <- NULL

      spec$paths$`/status/{id}`$get$responses$`200`$content$`application/json`$schema <- list(
        type = "object",
        required = c("id", "status"),
        properties = list(
          id = list(
            type = "string",
            description = "Identifier of the file conversion.",
            example = "17ecf252"
          ),
          status = list(
            type = "string",
            description = "Status of the conversion.",
            example = "pending"
          )
        )
      )

      spec$paths$`/status/{id}`$get$responses$`303`$content$`application/json`$schema <- list(
        type = "object",
        required = c("id", "status"),
        properties = list(
          id = list(
            type = "string",
            description = "Identifier of the file conversion.",
            example = "17ecf252"
          ),
          status = list(
            type = "string",
            description = "Status of the conversion.",
            example = "complete"
          )
        )
      )


      spec$paths$`/output/{id}`$get$description <-
        "Get the output file of a conversion using an identifier."

      spec$paths$`/output/{id}`$get$parameters[[1]]$example <- "17ecf252"

      spec$paths$`/output/{id}`$get$responses$`500`$content <- NULL
      spec$paths$`/output/{id}`$get$responses$default <- NULL

      spec$paths$`/healthz`$get$responses$`500`$content <- NULL
      spec$paths$`/healthz`$get$responses$default <- NULL

      spec

    }
  )

  pr$setDocs(
    "rapidoc",
    bg_color = "#2691d9",
    text_color = "#ffffff",
    primary_color = "#2c3e50",
    render_style = "read",
    slots = paste0(
      '<img ',
      'slot="logo" ',
      'src="../public/logo.png" ',
      'width=36px style=\"margin-left:7px\"/>'
    ),
    heading_text = paste("FGC", version),
    regular_font = "Roboto, Helvetica Neue, Helvetica, Arial, sans-serif",
    font_size = "largest",
    sort_tags = "false",
    sort_endpoints_by = "summary",
    allow_spec_file_load = "false"
  )

}
