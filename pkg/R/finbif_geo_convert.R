#' FinBIF geo-conversion
#'
#' Convert FinBIF data to geographic formats.
#'
#' @param input Character or Integer. Either the path to a Zip archive or
#'   tabular data file that has been downloaded from "laji.fi", a URI
#'   linking to such a data file (e.g.,
#'   [https://tun.fi/HBF.53254](https://tun.fi/HBF.53254)) or an integer
#'   representing the URI (i.e., `53254`).
#' @param output Character. Output file format in the form of a file extension.
#'   See `show_formats()` for a list of available file formats. Use `"none"`
#'   (default) to prevent writing output to a file.
#' @param geo Character. Geometry of output. One of `"point"`, `"bbox"` or "
#'   `"footprint"`.
#' @param crs Character or Integer. Coordinate reference system of output. One
#'   of `"euref"`, `"ykj"`, `"wgs84"` or integer indicating an EPSG code.
#' @param ... Other options passed to `finbif::finbif_occurrence_load`.
#'
#' @return An `{sf}` package simple feature object (invisibly). And if
#'   `output != "none"` then the spatial data file(s) will be written to the
#'   current working directory.
#'
#' @export
finbif_geo_convert <- function(
  input, output = "none", geo = c("point", "bbox", "footprint"), crs = "wgs84",
  ...
) {

  geo <- match.arg(geo)

  obj <- list(input = input, output = output, geo = geo, crs = crs)

  obj <- get_fmt(obj)

  obj <- get_input(obj, ...)

  obj <- sanitise_nms(obj)

  obj <- lgl2int(obj)

  obj <- get_spatial_input_nms(obj)

  obj <- points(obj)

  obj <- footprint(obj)

  obj <- project(obj)

  obj <- buffer(obj)

  obj <- bbox(obj)

  write_file(obj)

}

#' @noRd
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
get_fmt <- function(obj) {

  obj[["fmt"]] <- switch(
    obj[["output"]], none = obj[["output"]], tools::file_ext(obj[["output"]])
  )

  error_if(
    !obj[["fmt"]] %in% c("none", names(fmts)), "Format not supported",
    "not_supported"
  )

  obj[["output"]] <- tools::file_path_sans_ext(obj[["output"]])

  obj

}

#' @noRd
#' @importFrom finbif fb_occurrence_load
get_input <- function(obj, ...) {

  obj[["col_type"]] <- switch(obj[["fmt"]], shp = "short", "all")

  obj[["data"]] <- finbif::finbif_occurrence_load(
    obj[["input"]],
    select = c(obj[["col_type"]], paste0("-", deselect)),
    facts = facts,
    ...
  )

  obj[["n_rows"]] <- attr(obj[["data"]], "nrec_avl")

  obj[["col_names"]] <- names(obj[["data"]])

  obj

}

#' @noRd
#' @importFrom stringi stri_trans_general
sanitise_nms <- function(obj) {

  names(obj[["data"]]) <- gsub("\\s", "_", names(obj[["data"]]))

  names(obj[["data"]]) <- gsub("\\W", "", names(obj[["data"]]))

  if (identical(obj[["fmt"]] , "shp")) {

    names(obj[["data"]]) <- stringi::stri_trans_general(
      names(obj[["data"]]), "Latin-ASCII"
    )

  }

  obj

}

#' @noRd
lgl2int <- function(obj) {

  lgl_cols <- vapply(obj[["data"]], is.logical, NA)

  obj[["data"]][, lgl_cols] <- lapply(obj[["data"]][, lgl_cols], as.integer)

  obj

}

#' @noRd
get_spatial_input_nms <- function(obj) {

  type <- obj[["col_type"]]

  obj[["input_nms_lat"]] <- input_nms[[type]][["points"]][["lat"]]

  obj[["input_nms_lon"]] <- input_nms[[type]][["points"]][["lon"]]

  obj[["input_nms_footprint"]] <- input_nms[[type]][["footprint"]]

  obj

}

#' @noRd
points <- function(obj) {

  lat <- obj[["input_nms_lat"]]

  lon <- obj[["input_nms_lon"]]

  obj[["has_points"]] <- all(c(lat, lon) %in% obj[["col_names"]])

  if (identical(obj[["geo"]], "point")) {

    if (obj[["has_points"]]) {

      obj <- get_points_from_points(obj)

    } else {

      obj <- get_points_from_footprint(obj)

    }

  }

  obj

}

#' @noRd
#' @importFrom dplyr mutate rowwise select ungroup
#' @importFrom rlang .data
#' @importFrom sf st_as_sfc st_geometry st_is_empty st_point
#' @importFrom stringi stri_replace_na
get_points_from_points <- function(obj) {

  lat <- obj[["input_nms_lat"]]

  lon <- obj[["input_nms_lon"]]

  footprint <- obj[["input_nms_footprint"]]

  obj[["data"]] <- dplyr::rowwise(obj[["data"]])

  obj[["data"]] <- dplyr::mutate(
    obj[["data"]], geo = list(sf::st_point(c(.data[[lon]], .data[[lat]])))
  )

  obj[["data"]][c(lat, lon)] <- NULL

  obj[["data"]] <- dplyr::ungroup(obj[["data"]])

  obj[["data"]] <- dplyr::mutate(
    obj[["data"]], geo = sf::st_as_sfc(.data[["geo"]], crs = 4326L)
  )

  missing_points <- sf::st_is_empty(obj[["data"]][["geo"]])

  if (any(missing_points)) {

    sub_points <- obj[["data"]][[footprint]][missing_points]

    if (any(!is.na(sub_points))) {

      sub_points <- stringi::stri_replace_na(sub_points, "POINT EMPTY")

      sub_points <- sf::st_as_sfc(sub_points, crs = 4326L)

      sub_points <- footprint_to_points(sub_points)

      obj[["data"]][["geo"]][missing_points] <- sub_points

    }

  }

  obj[["data"]][[footprint]] <- NULL

  sf::st_geometry(obj[["data"]]) <- "geo"

  obj

}

#' @noRd
#' @importFrom sf st_geometry
get_points_from_footprint <- function(obj) {

  obj <- to_footprint(obj)

  lat <- obj[["input_nms_lat"]]

  lon <- obj[["input_nms_lon"]]

  footprint <- obj[["input_nms_footprint"]]

  obj[["data"]][["geo"]] <- footprint_to_points(obj[["data"]][[footprint]])

  obj[["data"]][c(lat, lon, footprint)] <- NULL

  sf::st_geometry(obj[["data"]]) <- "geo"

  obj

}

#' @noRd
#' @importFrom sf st_geometry st_is_empty
footprint <- function(obj) {

  if (!identical(obj[["geo"]], "point")) {

    lat <- obj[["input_nms_lat"]]

    lon <- obj[["input_nms_lon"]]

    footprint <- obj[["input_nms_footprint"]]

    obj <- to_footprint(obj)

    if (obj[["has_points"]]) {

      missing_footprints <- sf::st_is_empty(obj[["data"]][[footprint]])

      if (any(missing_footprints)) {

        sub_footprints <- obj[["data"]][[footprint]][missing_footprints]

        if (any(!is.na(sub_footprints))) {

          obj[["data"]][[footprint]][missing_footprints] <- sub_footprints

        }

      }

    }

    obj[["data"]][["geo"]] <- obj[["data"]][[footprint]]

    obj[["data"]][c(lat, lon, footprint)] <- NULL

    sf::st_geometry(obj[["data"]]) <- "geo"

  }

  obj

}

#' @noRd
#' @importFrom sf st_as_sfc st_bbox st_geometry_type st_polygon
bbox <- function(obj) {

  if (identical(obj[["geo"]], "bbox")) {

    bbox <- lapply(obj[["data"]][["geo"]], sf::st_bbox)

    bbox <- lapply(
      bbox,
      function(x) if (is.na(x)) sf::st_polygon() else sf::st_as_sfc(x)[[1L]]
    )

    crs <- switch(obj[["crs"]], euref = 3067L, wgs84 = 4326L)

    obj[["data"]][["geo"]] <- sf::st_as_sfc(bbox, crs = crs)

  }

  obj

}

#' @noRd
#' @importFrom sf st_transform
project <- function(obj) {

  if (identical(obj[["crs"]], "euref")) {

    obj[["data"]] <- sf::st_transform(obj[["data"]], crs = 3067L)

  }

  obj

}

#' @noRd
#' @importFrom sf st_buffer st_transform
buffer <- function(obj) {

  if (identical(obj[["geo"]], "bbox")) {

    transform <- identical(obj[["crs"]], "wgs84")

    is_point <- sf::st_geometry_type(obj[["data"]][["geo"]]) == "POINT"

    points <- obj[["data"]][["geo"]][is_point]

    if (transform) {

      points <- sf::st_transform(points, crs = 3067L)

    }

    points <- sf::st_buffer(points, .5, 1L)

    if (transform) {

      points <- sf::st_transform(points, crs = 4326L)

    }

    obj[["data"]][["geo"]][is_point] <- points

  }

  obj

}

#' @noRd
#' @importFrom sf st_as_sfc st_geometry_type st_transform
#' @importFrom stringi stri_replace_na
to_footprint <- function(obj) {

  footprint <- obj[["input_nms_footprint"]]

  missing_geo_err_msg <- paste0(
    "Geometric data for the requested geometry type is not avaiable for this ",
    "dataset."
  )

  error_if(
    is.null(obj[["data"]][[footprint]]), missing_geo_err_msg,
    "geometry_not_available"
  )

  obj[["data"]][[footprint]] <- stringi::stri_replace_na(
    obj[["data"]][[footprint]], "POLYGON EMPTY"
  )

  obj[["data"]][[footprint]] <- sf::st_as_sfc(
    obj[["data"]][[footprint]], crs = 4326L
  )

  gc <- geometry_type_chr(obj[["data"]][[footprint]]) == "GEOMETRYCOLLECTION"

  if (identical(obj[["geo"]], "footprint") && any(gc)) {

    uncollected <- sf::st_transform(obj[["data"]][[footprint]][gc], crs = 3067L)

    uncollected <- lapply(uncollected, uncollect)

    uncollected <- sf::st_as_sfc(uncollected, crs = 3067L)

    obj[["data"]][[footprint]][gc] <- sf::st_transform(uncollected, crs = 4326L)

  }

  if (identical(obj[["geo"]], "footprint")) {

    obj[["data"]][[footprint]] <- lapply(
      obj[["data"]][[footprint]], cast_to_multi
    )

    obj[["data"]][[footprint]] <- sf::st_as_sfc(
      obj[["data"]][[footprint]], crs = 4326L
    )

  }

  obj

}

#' @noRd
#' @importFrom sf st_multilinestring st_multipoint st_multipolygon
uncollect <- function(x) {

  gtype <- geometry_type_chr(x)

  if (identical(gtype, "GEOMETRYCOLLECTION")) {

    cgtypes <- vapply(x, geometry_type_chr, character(1L))

    utype <- unique(sub("^MULTI", "", cgtypes))

    if (identical(length(utype), 1L)) {

      x <- switch(
        utype,
        "POINT" = sf::st_multipoint(matrix(unlist(x), ncol = 2L, byrow = TRUE)),
        "LINESTRING" = sf::st_multilinestring(x),
        "POLYGON" = sf::st_multipolygon(x),
        x
      )

    } else {

      x <- lapply(x, to_polygon)

      x <- sf::st_multipolygon(x)

    }

  }

  x

}

#' @noRd
#' @importFrom sf st_cast
cast_to_multi <- function(x) {

  gtype <- geometry_type_chr(x)

  if (!grepl("MULTI", gtype)) {

    x <- sf::st_cast(x, paste0("MULTI", gtype))

  }

  x

}

#' @noRd
#' @importFrom sf st_buffer
to_polygon <- function(x) {

  geometries <- c("LINESTRING", "POINT", "MULTILINESTRING", "MULTIPOINT")

  if (geometry_type_chr(x) %in% geometries) {

    x <- sf::st_buffer(x, .5, 1L)

  }

  x

}

#' @noRd
#' @importFrom sf st_geometry_type
geometry_type_chr <- function(x) {

  as.character(sf::st_geometry_type(x))

}

#' @noRd
#' @importFrom sf st_centroid st_as_sfc
footprint_to_points <- function(x) {

  x <- lapply(x, sf::st_centroid)

  sf::st_as_sfc(x, crs  = 4326L)

}

#' @noRd
write_file <- function(obj) {

  obj <- switch(
    obj[["fmt"]],
    none = obj,
    rds = write_rds_file(obj),
    shp = write_shp_file(obj),
    write_gdal_file(obj)
  )

  out <- obj[["data"]]

  attr(out, "output") <- obj[["output"]]

  attr(out, "n_rows") <- obj[["n_rows"]]

  attr(out, "geo_types") <- obj[["geo_types"]]

  invisible(out)

}

#' @noRd
write_rds_file <- function(obj) {

  saveRDS(obj[["data"]], file = paste(obj[["output"]], obj[["fmt"]], sep = "."))

  obj

}

#' @noRd
#' @importFrom sf st_write
write_shp_file <- function(obj) {

  obj <- split_data_by_gtype(obj)

  for (i in seq_along(obj[["data"]])) {

    sf::st_write(
      obj[["data"]][[i]],
      sprintf(
        "%s_%s.%s", obj[["output"]], obj[["geo_types"]][[i]], obj[["fmt"]]
      ),
      layer_options = "ENCODING=UTF-8",
      quiet = TRUE
    )

  }

  obj

}

#' @noRd
#' @importFrom sf st_write
write_gdal_file <- function(obj) {

  obj <- split_data_by_gtype(obj)

  for (i in seq_along(obj[["data"]])) {

    sf::st_write(
      obj[["data"]][[i]],
      paste(obj[["output"]], obj[["fmt"]], sep = "."),
      layer = paste(
        gsub("\\.", "_", basename(obj[["output"]])),
        obj[["geo_types"]][[i]],
        sep = '_'
      ),
      quiet = TRUE
    )

  }

  obj

}

#' @noRd
split_data_by_gtype <- function(obj) {

  geo_types <- geometry_type_chr(obj[["data"]])

  obj[["geo_types"]] <- unique(geo_types)

  shp_incompatible <- anyNA(match(obj[["geo_types"]], shp_fmt_types))

  error_if(
    shp_incompatible && identical(obj[["fmt"]], "shp"),
    "Geometry too complex for '.shp' file. Please select another format.",
    "too_complex"
  )

  data <- list()

  for (i in obj[["geo_types"]]) {

    data[[i]] <- obj[["data"]][geo_types == i, ]

  }

  obj[["geo_types"]] <- sub("^multi", "", tolower(obj[["geo_types"]]))

  obj[["data"]] <- data

  obj

}

#' @noRd
deselect <- c(
  "lon_min_wgs84", "lat_min_wgs84", "lon_max_wgs84", "lat_max_wgs84",
  "lat_euref", "lon_euref", "lon_min_euref", "lat_min_euref",
  "lon_max_euref", "lat_max_euref", "lon_min_ykj", "lat_min_ykj",
  "lon_max_ykj", "lat_max_ykj", "coordinates_euref"
)

#' @noRd
facts <- list(
  record = c(
    "Havainnon laatu",
    "Havainnon määrän yksikkö",
    "Museo, johon lajista kerätty näyte on talletettu"
  ),
  event = c(
    "Vesistöalue",
    "Sijainnin tarkkuusluokka",
    "Pesintätulos"
  ),
  document = "Seurattava laji"
)

#' @noRd
input_nms <- list(
  short = list(
    points =  c(lat = "latWGS84", lon = "lonWGS84"),
    footprint = "fprntWGS84"
  ),
  all = list(
    points = c(lat = "lat_wgs84", lon = "lon_wgs84"),
    footprint = "footprint_wgs84"
  )
)

#' @noRd
shp_fmt_types <- c(
  "POINT", "POLYGON", "LINESTRING", "MULTIPOINT", "MULTILINESTRING",
  "MULTIPOLYGON"
)
