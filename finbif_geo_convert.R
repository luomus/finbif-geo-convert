library(finbif)
library(sf)
library(dplyr)
library(tidyr)
library(tools)
library(rlang)
library(stringi)
library(purrr)

bb <- function(x0, y0, x1, y1) {
  ans <- c(x0, x0, x1, x1, x0, y0, y1, y1, y0, y0)
  if (anyNA(ans)) return(NA)
  ans <- matrix(ans, 5L)
  list(st_polygon(list(ans)))
}

fmts <- c(
  "csv", "fgb", "geojson", "gml", "gmt", "gpkg", "gxt", "jml", "nc",
  "ods", "rds", "shp", "sqlite", "vdv", "xlsx", "none"
)

short_geo_col_nms <- c(
  "lonWGS84", "latWGS84", "lonEUREF", "latEUREF", "lon1KKJ",
  "lat1KKJ", "lon10KKJ", "lat10KKJ", "lonMnWGS84", "latMnWGS84",
  "lonMxWGS84", "latMxWGS84", "lonMnEUREF", "latMnEUREF",
  "lonMxEUREF", "latMxEUREF", "lonMnKKJ", "latMnKKJ",
  "lonMxKKJ", "latMxKKJ", "fprntWGS84"
)

geo_components <- list(
  point_wgs84 = c("lon_wgs84", "lat_wgs84"),
  point_euref = c("lon_euref", "lat_euref"),
  point_1km_kkj = c("lon_1_kkj", "lat_1_kkj"),
  point_10km_kkj = c("lon_10_kkj", "lat_10_kkj"),
  bbox_wgs84 = c(
    "lon_min_wgs84", "lat_min_wgs84", "lon_max_wgs84", "lat_max_wgs84"
  ),
  bbox_euref = c(
    "lon_min_euref", "lat_min_euref", "lon_max_euref", "lat_max_euref"
  ),
  bbox_kkj = c("lon_min_kkj", "lat_min_kkj", "lon_max_kkj", "lat_max_kkj"),
  footprint_wgs84 = "footprint_wgs84"
)

finbif_geo_convert <- function(
  input, output = "none", geo = "point", agg = NULL, crs = "wgs84", n = -1, ...
) {

  fmt <- switch(output, none = output, file_ext(output))

  stopifnot("Format not supported" = fmt %in% fmts)

  geo_col_names <- unlist(geo_components)

  geo_cols <- switch(fmt, shp = short_geo_col_nms, geo_col_names)

  names(geo_cols) <- geo_col_names

  agg <- switch(geo, point = agg, NULL)

  geo <- paste(c(geo, agg), collapse = "_")

  geo_crs <- paste(c(geo, crs), collapse = "_")

  geo_crs_is_avail <- geo_crs %in% names(geo_components)

  geo_crs_avail <- geo_crs

  if (!geo_crs_is_avail) {

    geo_crs_avail <- switch(
      geo,
      bbox = "euref",
      point = "euref",
      point_1km = "kkj",
      point_10km = "kkj",
      footprint = "wgs84"
    )

    geo_crs_avail <- paste(geo, geo_crs_avail, sep = "_")

  }

  spatial_data <- fb_occurrence_load(
    input, select = geo_components[[geo_crs_avail]], n = n, quiet = TRUE,
    keep_tsv = TRUE
  )

  input <- switch(
    file_ext(input),
    tsv = input,
    zip = sprintf(
      "%s/rows_%s.tsv", tempdir(), basename(file_path_sans_ext(input))
    ),
    sprintf(
      "%s/rows_HBF.%s.tsv", tempdir(),
      stri_extract_all_regex(input, "\\d+")[[1L]]
    )
  )

  spatial_data <- rowwise(spatial_data)

  spatial_data <- switch(
    geo_crs_avail,
    point_wgs84 = mutate(
      spatial_data, point_wgs84 = list(st_point(c(lon_wgs84, lat_wgs84)))
    ),
    point_euref = mutate(
      spatial_data, point_euref = list(st_point(c(lon_euref, lat_euref)))
    ),
    point_1km_kkj = mutate(
      spatial_data, point_1km_kkj = list(st_point(c(lon_1_kkj, lat_1_kkj)))
    ),
    point_10km_kkj = mutate(
      spatial_data, point_10km_kkj = list(st_point(c(lon_10_kkj, lat_10_kkj)))
    ),
    bbox_wgs84 = mutate(
      spatial_data,
      bbox_wgs84 = bb(
        lon_min_wgs84, lat_min_wgs84, lon_max_wgs84, lat_max_wgs84
      )
    ),
    bbox_euref = mutate(
      spatial_data,
      bbox_euref = bb(
        lon_min_euref, lat_min_euref, lon_max_euref, lat_max_euref
      )
    ),
    bbox_kkj = mutate(
      spatial_data,
      bbox_kkj = bb(lon_min_kkj, lat_min_kkj, lon_max_kkj, lat_max_kkj)
    ),
    footprint_wgs84 = mutate(
      spatial_data, footprint_wgs84 = replace_na(footprint_wgs84, "POINT EMPTY")
    )
  )

  spatial_data <- ungroup(spatial_data)

  spatial_data <- switch(
    geo_crs_avail,
    point_wgs84 = mutate(
      spatial_data, point_wgs84 = st_as_sfc(point_wgs84, crs = st_crs(4326))
    ),
    point_euref = mutate(
      spatial_data, point_euref = st_as_sfc(point_euref, crs = st_crs(3067))
    ),
    point_1km_kkj = mutate(
      spatial_data, point_1km_kkj = st_as_sfc(point_1km_kkj, crs = st_crs(2393))
    ),
    point_10km_kkj = mutate(
      spatial_data,
      point_10km_kkj = st_as_sfc(point_10km_kkj, crs = st_crs(2393))
    ),
    bbox_wgs84 = mutate(
      spatial_data, bbox_wgs84 = st_as_sfc(bbox_wgs84, crs = st_crs(4326))
    ),
    bbox_euref = mutate(
      spatial_data, bbox_euref = st_as_sfc(bbox_euref, crs = st_crs(3067))
    ),
    bbox_kkj = mutate(
      spatial_data, bbox_kkj = st_as_sfc(bbox_kkj, crs = st_crs(2393))
    ),
    footprint_wgs84 = mutate(
      spatial_data,
      footprint_wgs84 = st_as_sfc(footprint_wgs84, crs = st_crs(4326))
    )
  )

  spatial_data <- switch(
    geo_crs_avail,
    footprint_wgs84 = spatial_data,
    select(spatial_data, !any_of(geo_components[[geo_crs_avail]]))
  )

  if (!geo_crs_is_avail) {

    crs <- switch(
      as.character(crs), euref = 3067, kkj = 2393, wgs84 = 4326, crs
    )

    spatial_data[[geo_crs_avail]] <- st_transform(
      spatial_data[[geo_crs_avail]], st_crs(crs)
    )

    names(spatial_data) <- geo_crs

  }

  st_geometry(spatial_data) <- geo_crs

  data <- fb_occurrence_load(
    input,
    select = c(switch(fmt, shp = "short", "all"), paste0("-", geo_col_names)),
    n = n
  )

  data <- select(data, where(~any(!is.na(.x))))

  data <- mutate(data, across(where(is.logical), as.integer))

  if (identical(fmt, "shp")) {

    geo_types <- as.character(st_geometry_type(spatial_data))

    unique_geo_types <- unique(geo_types)

    data_list <- list()

    for (i in unique_geo_types) {

      ind <- geo_types == i

      data_list[[i]] <- data[ind, ]

      data_list[[i]] <- st_as_sf(
        cbind(data_list[[i]], spatial_data[ind, ]),
        agr = set_names(
          rep_len("identity", length(data_list[[i]])), names(data_list[[i]])
        )
      )

    }

  } else {

    data <- st_as_sf(
      cbind(data, spatial_data),
      agr = set_names(rep_len("identity", length(data)), names(data))
    )

  }

  switch(
    fmt,
    none = NULL,
    rds = saveRDS(data, output, ...),
    shp = shp_write(data_list, output, ...),
    st_write(data, output, ...)
  )

  invisible(data)

}

shp_write <- function(data, output, ...) {

  if (identical(length(data), 1L)) {

    names(data) <- output

  } else {

    names(data) <- sprintf(
      gsub("\\.shp$", "_%s.shp", output), tolower(names(data))
    )

  }

  for (i in names(data)) {

    st_write(data[[i]], i, layer_options = "ENCODING=UTF-8", ...)

  }

}

library(plumber)
p <- plumb("api.R")
p$run(host = "0.0.0.0", port = 8000L)
