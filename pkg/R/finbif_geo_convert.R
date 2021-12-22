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
#' @param agg Character. Aggregate features to a grid in KKJ CRS. One of
#'  `"1km"`, `"10km"`, `"1km_center"` or `"10km_center"`. Using the suffix
#'   `"_center"` will aggregate based on the center point of the feature's
#'    bounding box. Without, the aggregation is based on the entire bounding
#'    box and those features with bounding boxes that encompass multiple grid
#'    vertices will return NA.
#' @param crs Character or Integer. Coordinate reference system of output. One
#'   of `"euref"`, `"kkj"`, `"wgs84"` or integer indicating an EPSG code.
#' @param select Character. Variables to include in the output attribute table.
#'   Default is keyword `"all"` indicating all (non spatial) variables from
#'   input data will be included in the output.
#' @param n Integer. How many features to include. Negative and other invalid
#'   values are ignored causing all features to be included.
#' @param facts List. A named list of "facts" to extract from supplementary
#'   "fact" files in an input data archive. Names can include one or more of
#'   `"record"`, `"event"` or `"document"`. Elements of the list are character
#'   vectors of the "facts" to be extracted and then joined to the return value.
#' @param filetype Character. One of `"citable"` or `"lite"`. The type of input
#'  file. Only required if `select != "all"`.
#' @param locale Character. One of `"en"`. `"fi"` or `"sv"`. The locale of input
#'  file. Only required if `select != "all"` and `filetype == "lite"`.
#' @param ... Other options passed to `finbif::finbif_occurrence_load`.
#'
#' @return An `{sf}` package simple feature object (invisibly). And if
#'   `output != "none"` then the spatial data file(s) will be written to the
#'   current working directory.
#'
#' @importFrom dplyr across mutate rowwise transmute ungroup
#' @importFrom finbif fb_occurrence_load from_schema
#' @importFrom sf st_as_sf st_as_sfc st_bbox st_centroid st_crs st_geometry
#' @importFrom sf st_geometry_type st_point st_transform
#' @importFrom stringi stri_extract_all_regex stri_replace_na
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @export
finbif_geo_convert <- function(
  input, output = "none", geo = c("point", "bbox", "footprint"), agg = NULL,
  crs = "wgs84", select = "all", n = -1, facts = list(), filetype = "citable",
  locale = "en", ...
) {

  fmt <- switch(output, none = output, tools::file_ext(output))

  stopifnot("Format not supported" = fmt %in% c("none", names(fmts)))

  geo_col_names <- unlist(geo_components)

  geo_cols <- switch(fmt, shp = short_geo_col_nms, geo_col_names)

  names(geo_cols) <- geo_col_names

  geo <- match.arg(geo)

  agg <- switch(geo, point = agg, NULL)

  geo <- paste(c(geo, agg), collapse = "_")

  geo_crs <- paste(c(geo, crs), collapse = "_")

  geo_crs_is_avail <- geo_crs %in% names(geo_components)

  geo_crs_avail <- geo_crs

  if (!geo_crs_is_avail) {

    geo_crs_avail <- switch(
      geo,
      bbox = "wgs84",
      point = "wgs84",
      point_1km = "kkj",
      point_10km = "kkj",
      point_1km_center = "kkj",
      point_10km_center = "kkj",
      footprint = "wgs84"
    )

    geo_crs_avail <- paste(geo, geo_crs_avail, sep = "_")

  }

  vars_with_data <- finbif::finbif_occurrence_load(
    input, select = "all", n = 1L, drop_na = TRUE, keep_tsv = TRUE,
    facts = facts, quiet = TRUE
  )

  vars_with_data <- names(vars_with_data)

  geo_cols_req <- geo_components[[geo_crs_avail]]

  footprint_req <- crs != "wgs84" && is.null(agg)

  has_geo_data <- geo_cols_req %in% vars_with_data

  has_geo_data <- all(
    has_geo_data, "footprint_wgs84" %in% vars_with_data || !footprint_req
  )

  err_msg <- paste0(
    "Geometric data for the requested geometry type is not avaiable for this ",
    "dataset."
  )

  names(has_geo_data) <- err_msg

  do.call(stopifnot, as.list(has_geo_data))

  combined_facts <- list()

  for (i in names(default_facts)) {

    combined_facts[[i]] <- c(default_facts[[i]], facts[[i]])

  }

  facts <- combined_facts

  geo_cols_req <- unique(c(geo_cols_req, "footprint_wgs84"))

  spatial_data <- finbif::finbif_occurrence_load(
    input, select = geo_cols_req, n = n, quiet = TRUE
  )

  input <- switch(
    tools::file_ext(input),
    tsv = input,
    ods = input,
    xlsx = input,
    zip = sprintf(
      "%s/rows_%s.tsv",
      dirname(input),
      basename(tools::file_path_sans_ext(input))
    ),
    sprintf(
      "%s/rows_HBF.%s.tsv",
      tempdir(),
      stringi::stri_extract_all_regex(input, "\\d+")[[1L]]
    )
  )

  spatial_data <- dplyr::rowwise(spatial_data)

  spatial_data <- switch(
    geo_crs_avail,
    point_wgs84 = dplyr::mutate(
      spatial_data, point_wgs84 = list(sf::st_point(c(lon_wgs84, lat_wgs84)))
    ),
    point_euref = dplyr::mutate(
      spatial_data, point_euref = list(sf::st_point(c(lon_euref, lat_euref)))
    ),
    point_1km_kkj = dplyr::mutate(
      spatial_data,
      point_1km_kkj = list(sf::st_point(c(lon_1_kkj * 1e3, lat_1_kkj * 1e3)))
    ),
    point_10km_kkj = dplyr::mutate(
      spatial_data,
      point_10km_kkj = list(sf::st_point(c(lon_10_kkj * 1e4, lat_10_kkj * 1e4)))
    ),
    point_1km_center_kkj = dplyr::mutate(
      spatial_data,
      point_1km_center_kkj = list(
        sf::st_point(c(lon_1_center_kkj * 1e3, lat_1_center_kkj * 1e3))
      )
    ),
    point_10km_center_kkj = dplyr::mutate(
      spatial_data,
      point_10km_center_kkj = list(
        sf::st_point(c(lon_10_center_kkj * 1e4, lat_10_center_kkj * 1e4))
      )
    ),
    bbox_wgs84 = dplyr::mutate(
      spatial_data,
      bbox_wgs84 = bb(
        lon_min_wgs84, lat_min_wgs84, lon_max_wgs84, lat_max_wgs84
      )
    ),
    bbox_euref = dplyr::mutate(
      spatial_data,
      bbox_euref = bb(
        lon_min_euref, lat_min_euref, lon_max_euref, lat_max_euref
      )
    ),
    bbox_kkj = dplyr::mutate(
      spatial_data,
      bbox_kkj = bb(lon_min_kkj, lat_min_kkj, lon_max_kkj, lat_max_kkj)
    ),
    spatial_data
  )

  spatial_data <- dplyr::mutate(
    spatial_data,
    footprint_wgs84 = stringi::stri_replace_na(footprint_wgs84, "POINT EMPTY")
  )

  spatial_data <- dplyr::ungroup(spatial_data)

  spatial_data <- switch(
    geo_crs_avail,
    point_wgs84 = dplyr::mutate(
      spatial_data,
      point_wgs84 = sf::st_as_sfc(point_wgs84, crs = sf::st_crs(4326))
    ),
    point_euref = dplyr::mutate(
      spatial_data,
      point_euref = sf::st_as_sfc(point_euref, crs = sf::st_crs(3067))
    ),
    point_1km_kkj = dplyr::mutate(
      spatial_data,
      point_1km_kkj = sf::st_as_sfc(point_1km_kkj, crs = sf::st_crs(2393))
    ),
    point_10km_kkj = dplyr::mutate(
      spatial_data,
      point_10km_kkj = sf::st_as_sfc(point_10km_kkj, crs = sf::st_crs(2393))
    ),
    point_1km_center_kkj = dplyr::mutate(
      spatial_data,
      point_1km_center_kkj = sf::st_as_sfc(
        point_1km_center_kkj, crs = sf::st_crs(2393)
      )
    ),
    point_10km_center_kkj = dplyr::mutate(
      spatial_data,
      point_10km_center_kkj = sf::st_as_sfc(
        point_10km_center_kkj, crs = sf::st_crs(2393)
      )
    ),
    bbox_wgs84 = dplyr::mutate(
      spatial_data,
      bbox_wgs84 = sf::st_as_sfc(bbox_wgs84, crs = sf::st_crs(4326))
    ),
    bbox_euref = dplyr::mutate(
      spatial_data,
      bbox_euref = sf::st_as_sfc(bbox_euref, crs = sf::st_crs(3067))
    ),
    bbox_kkj = dplyr::mutate(
      spatial_data, bbox_kkj = sf::st_as_sfc(bbox_kkj, crs = sf::st_crs(2393))
    ),
    spatial_data
  )

  spatial_data <- dplyr::mutate(
    spatial_data,
    footprint_wgs84 = sf::st_as_sfc(footprint_wgs84, crs = sf::st_crs(4326))
  )

  spatial_data <- switch(
    geo_crs_avail,
    point_euref = dplyr::mutate(
      spatial_data,
      footprint_euref = sf::st_transform(
        footprint_wgs84, crs = sf::st_crs(3067)
      )
    ),
    bbox_euref = dplyr::mutate(
      spatial_data,
      footprint_euref = sf::st_transform(
        footprint_wgs84, crs = sf::st_crs(3067)
      )
    ),
    bbox_kkj = dplyr::mutate(
      spatial_data,
      footprint_kkj = sf::st_transform(footprint_wgs84, crs = sf::st_crs(2393))
    ),
    spatial_data
  )

  spatial_data <- dplyr::rowwise(spatial_data)

  spatial_data <- switch(
    geo_crs_avail,
    point_euref = dplyr::mutate(
      spatial_data,
      point_euref_ = sf::st_centroid(footprint_euref),
      point_euref__ = list(
        ifelse(is.na(point_euref_), point_euref, point_euref_)
      ),
      point_euref = sf::st_as_sfc(point_euref__, crs = sf::st_crs(3067))
    ),
    bbox_euref = dplyr::mutate(
      spatial_data,
      bbox_euref_ = list(sf::st_bbox(footprint_euref)),
      bbox_euref__ = sf::st_as_sfc(bbox_euref_),
      bbox_euref___ = list(
        ifelse(is.na(bbox_euref__), bbox_euref, bbox_euref__)
      ),
      bbox_euref = sf::st_as_sfc(bbox_euref___, crs = sf::st_crs(3067))
    ),
    bbox_kkj = dplyr::mutate(
      spatial_data,
      bbox_kkj_ = list(sf::st_bbox(footprint_kkj)),
      bbox_kkj__ = sf::st_as_sfc(bbox_kkj_),
      bbox_kkj___ = list(ifelse(is.na(bbox_kkj__), bbox_kkj, bbox_kkj__)),
      bbox_kkj = sf::st_as_sfc(bbox_kkj___, crs = sf::st_crs(2393))
    ),
    spatial_data
  )

  spatial_data <- dplyr::ungroup(spatial_data)

  keep_cols <- intersect(
    c(geo_crs_avail, "footprint_wgs84"), names(spatial_data)
  )

  spatial_data <- spatial_data[, keep_cols]

  sf::st_geometry(spatial_data) <- geo_crs_avail

  if ("GEOMETRYCOLLECTION" %in% sf::st_geometry_type(spatial_data)) {

    spatial_data <- switch(
      geo_crs_avail,
      footprint_wgs84 = dplyr::mutate(
        spatial_data,
        footprint_wgs84 = sf::st_as_sfc(
          lapply(footprint_wgs84, uncollect), crs = sf::st_crs(4326)
        )
      ),
      spatial_data
    )

  }

  if (!geo_crs_is_avail) {

    footprint_crs <- paste0("footprint_", crs)

    crs <- switch(
      as.character(crs), euref = 3067, kkj = 2393, wgs84 = 4326, crs
    )

    if (identical(geo, "footprint") || !is.null(agg)) {

      spatial_data[[geo_crs_avail]] <- sf::st_transform(
        spatial_data[[geo_crs_avail]], sf::st_crs(crs)
      )

    } else {

      spatial_data[[footprint_crs]] <- sf::st_transform(
        spatial_data[["footprint_wgs84"]], sf::st_crs(crs)
      )

      if (identical(geo, "bbox")) {

        spatial_data[[geo_crs_avail]] <- do.call(
          c,
          lapply(
            spatial_data[[footprint_crs]],
            function(x) sf::st_as_sfc(sf::st_bbox(x))
          )
        )


      } else if (identical(geo, "point")) {

        spatial_data[[geo_crs_avail]] <- sf::st_centroid(
          spatial_data[[footprint_crs]]
        )

      }

    }

  }

  spatial_data <- spatial_data[, geo_crs_avail]

  names(spatial_data) <- geo_crs

  sf::st_geometry(spatial_data) <- geo_crs

  data <- finbif::finbif_occurrence_load(
    input,
    select = c(switch(fmt, shp = "short", "all"), paste0("-", geo_col_names)),
    n = n, facts = facts, ...
  )

  col_type <- "native"

  if (attr(data, "dwc")) {

    col_type <- "dwc"

  }

  to <- switch(fmt, shp = "short", col_type)

  data <- switch(
    select,
    all = data,
    data[
      ,
      finbif::from_schema(select, to = to, file = filetype, locale = locale)
    ]
  )

  data <- dplyr::mutate(data, dplyr::across(where(is.logical), as.integer))

  if (identical(fmt, "shp")) {

    geo_types <- as.character(sf::st_geometry_type(spatial_data))

    unique_geo_types <- unique(geo_types)

    stopifnot(
      "Geometry too complex for '.shp' file. Please select another format." =
      !anyNA(match(unique_geo_types, shp_fmt_types))
    )

    data_list <- list()

    for (i in unique_geo_types) {

      ind <- geo_types == i

      data_list[[i]] <- data[ind, ]

      data_list[[i]] <- sf::st_as_sf(
        cbind(data_list[[i]], spatial_data[ind, ]),
        agr = stats::setNames(
          rep_len("identity", length(data_list[[i]])), names(data_list[[i]])
        )
      )

    }

  } else {

    data <- sf::st_as_sf(
      cbind(data, spatial_data),
      agr = stats::setNames(rep_len("identity", length(data)), names(data))
    )

  }

  switch(
    fmt,
    none = NULL,
    rds = saveRDS(data, output),
    shp = shp_write(data_list, output),
    sf::st_write(data, output, quiet = TRUE)
  )

  invisible(data)

}

#' @noRd
short_geo_col_nms <- c(
  "lonWGS84", "latWGS84", "lonEUREF", "latEUREF", "lon1KKJ", "lat1KKJ",
  "lon10KKJ", "lat10KKJ", "lon1cKKJ", "lat1cKKJ", "lon10cKKJ", "lat10cKKJ",
  "lonMnWGS84", "latMnWGS84", "lonMxWGS84", "latMxWGS84", "lonMnEUREF",
  "latMnEUREF", "lonMxEUREF", "latMxEUREF", "lonMnKKJ", "latMnKKJ", "lonMxKKJ",
  "latMxKKJ", "fprntWGS84", "crd1KKJ", "crd1cKKJ", "crd10KKJ",
  "crd10cKKJ"
)

#' @noRd
geo_components <- list(
  point_wgs84 = c("lon_wgs84", "lat_wgs84"),
  point_euref = c("lon_euref", "lat_euref"),
  point_1km_kkj = c("lon_1_kkj", "lat_1_kkj"),
  point_10km_kkj = c("lon_10_kkj", "lat_10_kkj"),
  point_1km_center_kkj = c("lon_1_center_kkj", "lat_1_center_kkj"),
  point_10km_center_kkj = c("lon_10_center_kkj", "lat_10_center_kkj"),
  bbox_wgs84 = c(
    "lon_min_wgs84", "lat_min_wgs84", "lon_max_wgs84", "lat_max_wgs84"
  ),
  bbox_euref = c(
    "lon_min_euref", "lat_min_euref", "lon_max_euref", "lat_max_euref"
  ),
  bbox_kkj = c("lon_min_kkj", "lat_min_kkj", "lon_max_kkj", "lat_max_kkj"),
  footprint_wgs84 = "footprint_wgs84",
  c(
    "coordinates_1_kkj", "coordinates_1_center_kkj" ,"coordinates_10_kkj",
    "coordinates_10_center_kkj"
  )
)

#' @noRd
#' @importFrom sf st_polygon
bb <- function(x0, y0, x1, y1) {
  ans <- c(x0, x0, x1, x1, x0, y0, y1, y1, y0, y0)
  if (anyNA(ans)) return(NA)
  ans <- matrix(ans, 5L)
  list(sf::st_polygon(list(ans)))
}

#' @noRd
#' @importFrom tidyselect vars_select_helpers
where <- tidyselect::vars_select_helpers[["where"]]

#' @noRd
#' @importFrom sf st_write
#' @importFrom stringi stri_trans_general
shp_write <- function(data, output) {

  if (!identical(length(data), 1L)) {

    output <- sprintf(
      gsub("\\.shp$", "_%s.shp", output), tolower(names(data))
    )

  }

  names(data) <- output

  for (i in names(data)) {

    names(data[[i]]) <- stringi::stri_trans_general(
      names(data[[i]]), "Latin-ASCII"
    )

    st_write(data[[i]], i, layer_options = "ENCODING=UTF-8", quiet = TRUE)

  }

}

#' @noRd
shp_fmt_types <- c(
  "POINT", "POLYGON", "LINESTRING", "MULTIPOINT", "MULTILINESTRING",
  "MULTIPOLYGON"
)

#' @noRd
#' @importFrom sf st_geometry_type
geometry_type_chr <- function(x) {

  as.character(sf::st_geometry_type(x))

}

#' @noRd
#' @importFrom sf st_multilinestring st_multipoint st_multipolygon
uncollect <- function(x) {

  gtype <- geometry_type_chr(x)

  if (!identical(gtype, "GEOMETRYCOLLECTION")) {

    return(x)

  }

  cgtypes <- vapply(x, geometry_type_chr, character(1L))

  utype <- unique(sub("^MULTI", "", cgtypes))

  if (length(utype) > 1L) {

    return(x)

  }

  switch(
    utype,
    "POINT" = sf::st_multipoint(matrix(unlist(x), ncol = 2, byrow = TRUE)),
    "LINESTRING" = sf::st_multilinestring(x),
    "POLYGON" = sf::st_multipolygon(x),
    x
  )

}

#' @noRd
default_facts <- list(
  record = c(
    "Havainnon laatu", "Havainnon määrän yksikkö",
    "Museo, johon lajista kerätty näyte on talletettu"
  ),
  event = c("Vesistöalue", "Sijainnin tarkkuusluokka", "Pesintätulos"),
  document = c(
    "Aineistolähde", "Tietolähteen kuvaus", "Seurattava laji",
    "Lajinseurantakohteen tila"
  )
)
