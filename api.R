#* @apiTitle FinBIF Geographic Data Conversion API
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")
#* @apiTag convert Convert a FinBIF occurrence data file into a geographic data format
#* @apiTag status Check status of API

suppressPackageStartupMessages({

  library(fgc, quietly = TRUE)
  library(future, quietly = TRUE)
  library(later, quietly = TRUE)
  library(promises, quietly = TRUE)
  library(rapidoc, quietly = TRUE)
  library(tools, quietly = TRUE)
  library(utils, quietly = TRUE)

})

if (!dir.exists("logs/errors")) dir.create("logs/errors", recursive = TRUE)

future::plan("multicore", workers = 2L)

options(
  finbif_rate_limit = Inf,
  finbif_use_cache = FALSE,
  finbif_use_cache_metadata = TRUE
)

if (identical(Sys.getenv("BRANCH"), "dev")) {

  options(finbif_dl_url = "https://staging.laji.fi/laji-etl/download")

}

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

#* Convert FinBIF data file with persistent identifier
#* @get /<input:str>/<fmt:str>/<geo:str>/<crs:str>
#* @post /<input:str>/<fmt:str>/<geo:str>/<crs:str>
#* @param input:str Input file's identifier.
#* @param fmt:str The output file format (in the form of a file extension) for the geographic data.
#* @param geo:str The geometry type of the output. One of 'point', 'bbox' or 'footprint'.
#* @param crs:str The coordinate reference system for the output. One of "euref" or "wgs84".
#* @param file:file File to convert (maximum allowed size is ~100mb).
#* @param personToken:str For use with restricted data downloads.
#* @tag convert
#* @response 303 A json object
#* @serializer unboxedJSON
function(input, fmt, geo, crs, file = "", personToken = "", req, res) {

  id <- digest::digest(list(req, sample(1e9L, 1L)), "xxhash32")

  input <- fgc::sanitise_id(input)

  id <- paste(input[["name"]], id, sep = "-")

  dir.create(id)

  on.exit(later::later(~unlink(id, recursive = TRUE), 3600L))

  input_file <- switch(
    req[["REQUEST_METHOD"]],
    GET = input[["file"]],
    POST = paste0(id, "/", names(file)[[1L]])
  )

  promises::future_promise(
    {

      if (identical(req[["REQUEST_METHOD"]], "POST")) {

        writeBin(file[[1L]], input_file)

      }

      if (!identical(personToken, "")) {

        Sys.setenv(FINBIF_RESTRICTED_FILE_ACCESS_TOKEN = personToken)

        op <- options(
          finbif_dl_url = paste0(getOption("finbif_dl_url"), "/secured")
        )

        on.exit({
          options(op)
          Sys.unsetenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")
        })

      }

      orig_file <- "original_download.zip"

      orig_path <- paste0(id, "/", orig_file)

      res <- try(
        {

          input_name <- gsub("\\.", "_", input[["name"]])

          output_file_init <- paste0(id, "/", input_name, "_geo.", fmt)

          output_file <- output_file_init

          skip <- 0L
          n <- as.integer(Sys.getenv("MAX_CHUNK_SIZE", "1e5"))
          data <- list()
          attr(data, "n_rows") <- Inf
          acc <- character()
          fmt_name <- switch(fmt, shp = "'ESRI Shapefile'", "'gpkg'")

          progress_file <- file.path(id, "progress")

          while (skip < attr(data, "n_rows")) {

            cat(
              as.integer(skip / attr(data, "n_rows") * 10) * 10L,
              file = progress_file
            )

            data <- fgc::finbif_geo_convert(

              input_file, output_file, geo, crs, drop_na = FALSE,
              drop_facts_na = FALSE, quiet = TRUE, cache = FALSE,
              write_file = orig_path, n = n, skip = skip
            )

            if (length(acc) < 1L) {

              geo_types_combined <- attr(data, "geo_types")

            } else {

              if (!identical(fmt, "shp")) {

                error_code <- system2(
                  "ogr2ogr",
                  c(
                    "-f", fmt_name, "-update", "-append", output_file_init,
                    output_file
                  ),
                  stdout = FALSE,
                  stderr = FALSE
                )

                if (!identical(error_code, 0L)) {

                  stop(
                    "Could not combine files; err_name: combine_failed",
                    call. = FALSE
                  )

                }

              } else {

                for (i in seq_along(attr(data, "geo_types"))) {

                  args <- list(
                    file = sprintf(
                      "%s/%s_%s.%s", id, basename(attr(data, "output")),
                      attr(data, "geo_types")[[i]], fmt
                    ),
                    add_file = sprintf(
                      "%s/%s/%s_%s.%s", id, acc[[length(acc)]],
                      basename(attr(data, "output")),
                      attr(data, "geo_types")[[i]], fmt
                    ),
                    layer = sprintf(
                      "%s_%s", basename(attr(data, "output")),
                      attr(data, "geo_types")[[i]]
                    )
                  )

                  if (attr(data, "geo_types")[[i]] %in% geo_types_combined) {

                    error_code <- system2(
                      "ogr2ogr",
                      c(
                        "-f", fmt_name, "-update", "-append", args[["file"]],
                        args[["add_file"]], "-nln", args[["layer"]]
                      ),
                      stdout = FALSE,
                      stderr = FALSE
                    )

                  } else {

                    error_code <- system2(
                      "ogr2ogr",
                      c(
                        "-f", fmt_name, args[["file"]], args[["add_file"]],
                        "-nln", args[["layer"]]
                      ),
                      stdout = FALSE,
                      stderr = FALSE
                    )

                    geo_types_combined <- c(
                      geo_types_combined, attr(data, "geo_types")[[i]]
                    )

                  }

                  if (!identical(error_code, 0L)) {

                    stop(
                      "Could not combine files; err_name: combine_failed",
                      call. = FALSE
                    )

                  }

                }

              }

            }

            skip <- skip + n

            next_file <- paste0("additional_file_", length(acc) + 1L)

            acc <- c(acc, next_file)

            dir.create(file.path(id, next_file))

            output_file <- paste0(
              id, "/", next_file, "/", input_name, "_geo.", fmt
            )

          }

        },
        silent = TRUE
      )

      if (inherits(res, "try-error")) {

         err_file <- paste0(id, "/error.txt")

         writeLines(res[[1L]], err_file)

         file.copy(err_file, sprintf("logs/errors/%s.txt", id))

         if (file.exists(orig_path)) {

           file.copy(orig_path, sprintf("logs/errors/%s.zip", id))

         }

         if (file.exists(input_file)) {

           file.copy(
             input_file,
             sprintf("logs/errors/%s.%s", id, tools::file_ext(input_file))
           )

         }

      } else {

        if (file.exists(orig_path)) {

          orig_files <- unzip(orig_path, list = TRUE)

          orig_files <- orig_files[["Name"]]

          readme <- grep("^readme.*\\.txt$", orig_files, value = TRUE)

          unzip(orig_path, readme, exdir = id)

        }

        additional_files <- list.files(
          id, "additional_file_", full.names = TRUE
        )

        zip(
          paste0(id, "/", input_name, "_geo_", fmt, ".zip"),
          setdiff(
            list.files(id, full.names = TRUE),
            c(input_file, orig_path, additional_files, progress_file)
          ),
          flags = "-rj9qX"
        )

      }

    },
    globals = c(
      "input", "input_file", "fmt", "geo", "crs", "id", "file", "personToken"
    ),
    packages = "fgc"
  )

  res$status <- 303L
  res$setHeader("Location", paste0("/status/", id))

  c(id = id)

}

#* Get status of conversion
#* @get /status/<id:str>
#* @param id:str The identifier of a conversion.
#* @tag convert
#* @response 200 A json object
#* @response 303 A json object
#* @response 400 Client error
#* @response 404 File not found
#* @serializer unboxedJSON
function(id, res) {

  if (!dir.exists(id)) {

    res$status <- 404L
    return("File not found")

  }

  poll <- promises::future_promise(
    {

      status <- character()

      sleep <- .1

      timer <- 0

      while (length(status) < 1L) {

        status <- list.files(id, pattern = "_geo_.*\\.zip$")

        timer <- timer + sleep

        if (timer > 30L) {

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
    globals = "id"
  )

  promises::then(
    poll,
    ~{

      if (identical(., "pending")) {

         list(
           id = id,
           status = .,
           progress_percent = scan(file.path(id, "progress"), quiet = TRUE)
         )

      } else if (identical(., "error")) {

        res$status <- 400L

        error <- paste0(id, "/error.txt")

        msg <- readChar(error, file.info(error)$size)

        msg <- fgc::read_error_msg(msg)

        c(id = id, msg)

      } else {

        res$status <- 303L
        res$setHeader("Location", paste0("/output/", id))

        list(id = id, status = "complete", progress_percent = 100)

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

  zip <- list.files(id, pattern = "_geo_.*\\.zip$", full.names = TRUE)

  out <- readBin(zip, "raw", n = file.info(zip)$size)

  plumber::as_attachment(out, zip)

}

#* @assets figures
list()

#* @get /favicon.ico
#* @serializer contentType list(type="image/x-icon")
function() {

  readBin("favicon.ico", "raw", n = file.info("favicon.ico")$size)

}

#* @get /robots.txt
#* @serializer contentType list(type="text/plain")
function() {

  readBin("robots.txt", "raw", n = file.info("robots.txt")$size)

}

#* @plumber
function(pr) {

  version <- as.character(utils::packageVersion("fgc"))

  plumber::pr_set_api_spec(
    pr,
    function(spec) {

      spec$info$version <- version

      spec$info$description <- readChar("api.md", file.info("api.md")$size)

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$summary <-
        "Convert FinBIF data file via data upload"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$description <- paste0(
        "Convert a FinBIF citable or lite data file to a geographic data ",
        "format via upload."
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[1]]$description <- paste0(
        "An identifier for the geographic data file. ",
        "Will be used in the filename of the download."
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$parameters[[1]]$example <-
        switch(Sys.getenv("BRANCH"), dev = "HBF.6988", "HBF.53254")

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
            example = "file406c41a"
          )
        )
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[1]]$example <-
        "file406c41a"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[2]]$example <-
        "gpkg"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[3]]$example <-
        "point"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[4]]$example <-
        "wgs84"

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$requestBody <- NULL

      for (i in c("personToken")) {

        pars <- spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters
        ind <- which(vapply(pars, getElement, character(1L), "name") == i)
        spec$paths$`/{input}/{fmt}/{geo}/{crs}`$post$parameters[[ind]] <- NULL

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
            example = switch(
              Sys.getenv("BRANCH"), dev = "HBF.6988-20920cf1",
              "HBF.53254-20920cf1"
            )
          )
        )
      )

      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$`500`$content <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$default <- NULL
      spec$paths$`/{input}/{fmt}/{geo}/{crs}`$get$responses$`200` <- NULL

      spec$paths$`/status/{id}`$get$description <-
        "Get the status of a conversion using an identifier."

      spec$paths$`/status/{id}`$get$parameters[[1]]$example <- switch(
        Sys.getenv("BRANCH"), dev = "HBF.6988-20920cf1", "HBF.53254-20920cf1"
      )

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
            example = switch(
              Sys.getenv("BRANCH"), dev = "HBF.6988-20920cf1",
              "HBF.53254-20920cf1"
            )
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
            example = switch(
              Sys.getenv("BRANCH"), dev = "HBF.6988-20920cf1",
              "HBF.53254-20920cf1"
            )
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

      spec$paths$`/output/{id}`$get$parameters[[1]]$example <- switch(
        Sys.getenv("BRANCH"), dev = "HBF.6988-20920cf1", "HBF.53254-20920cf1"
      )

      spec$paths$`/output/{id}`$get$responses$`500`$content <- NULL
      spec$paths$`/output/{id}`$get$responses$default <- NULL

      spec$paths$`/healthz` <- NULL
      spec$paths$`/favicon.ico` <- NULL
      spec$paths$`/robots.txt` <- NULL

      spec

    }
  )

  pr$setDocs(
    "rapidoc",
    bg_color = "#141B15",
    text_color = "#FFFFFF",
    primary_color = "#55AAE2",
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
