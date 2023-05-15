#' Viewshed Visibility Index (VVI) from sf
#' @description The VVI expresses the proportion of visible area to the total area based on a \code{\link[GreenExp]{viewshed}}.
#' The estimated VVI values range between 0 and 1, where 0 = no visible cells, and 1 = all of the cells are visible.
#'
#' @param observer object of class \code{sf}; Observer location(s) from where the VVI should be computed. See ‘Details’ for valid sf geometry types
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dtm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DTM
#' @param max_distance numeric; Buffer distance to calculate the viewshed
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res optional; NULL or numeric > 0; Resolution that the GreenExp raster should be aggregated to. Needs to be a multible of the dsm_rast resolution
#' @param spacing optional; numeric > 0; Only if \code{observer} is a linestring (or polygon), points on the line (or on a grid) will be generated.
#' The \code{spacing} parameter sets the distance in between the points on the line/grid
#' @param cores numeric; The number of cores to use, i.e. at most how many child processes will be run simultaneously
#' @param folder_path optional; Folder path to where the output should be saved continuously. Must not inklude a filename extension (e.g. '.shp', '.gpkg').
#' @param progress logical; Show progress bar and computation time?
#' @param output_type A string. One of `"VVI"`, `"viewshed"` or `"cumulative"`
#' @param by_row Logical. Default `FALSE`. Whether or not to return the result separately for each row in `observer`.
#' Only useful for lines or polygons.
#'
#' @details
#' observer needs to be a geometry of type POINT, LINESTRING, MULTILINESTRING, POLYGON or MULTIPOLYGON. If observer is a LINESTRING or MULTILINESTRING,
#' points will be generated along the line(s) every "resolution" meters. If observer is a POLYGON or MULTIPOLYGON, a grid with resolution = "resolution"
#' will be generated, and VVI will be computed for every point.
#' The CRS (\code{\link[sf]{st_crs}}) needs to have a metric unit!
#'
#' @return
#' If `output_type` is `"VVI"`, an sf_object containing the VVI values as POINT features, where 0 = no visible cells, and 1 = all of the cells are visible.
#' If `output_type` is `"viewshed"`, a `SpatRaster` where cell values are equal to the number of times they are visible from observers.
#' These values range from 0 to the number of observer points (depending on spacing).
#' If `output_type` is `"cumulative"` and `by_row` is FALSE, a single number indicating the cumulative proportion of cells that are visible from at least one observer point inside the area determined by the union of observer points buffered by `max_distance`.
#' In case `by_row` is TRUE, this will be a data.frame with columns `rowid` (identifying each row in `observer`) and `cvvi` (the cumulative viewshed visibility index).
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_crs
#' @importFrom sf st_geometry_type
#' @importFrom sf st_union
#' @importFrom sf st_cast
#' @importFrom sf st_line_sample
#' @importFrom sf st_as_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_buffer
#' @importFrom sf st_coordinates
#' @importFrom sf st_as_sfc
#' @importFrom sf st_cast
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @importFrom terra crs
#' @importFrom terra res
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra vect
#' @importFrom terra xyFromCell
#' @importFrom terra terraOptions
#' @importFrom terra aggregate
#' @importFrom terra extract
#' @importFrom terra cellFromXY
#' @importFrom terra colFromX
#' @importFrom terra rowFromY
#' @importFrom terra writeRaster
#' @importFrom terra rast
#' @importFrom raster raster
#' @importFrom methods is
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @useDynLib GreenExp, .registration = TRUE

vvi_from_sf <- function(observer, dsm_rast, dtm_rast,
                        max_distance = 800, observer_height = 1.7,
                        raster_res = NULL, spacing = raster_res,
                        cores = 1, folder_path = NULL, progress = FALSE,
                        output_type = c("VVI", "viewshed", "cumulative"),
                        by_row = FALSE) {

  #### 1. Check input ####
  # observer
  valid_sf_types <- c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  if (!is(observer, "sf")) {
    stop("observer must be a sf object")
  } else if (is.null(sf::st_crs(observer)$units) || sf::st_crs(observer)$units_gdal == "degree") {
    stop("observer CRS unit must not be degree")
  } else if (!as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% valid_sf_types) {
    stop("observer has no valid geometry")
  } else if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) == "MULTIPOINT") {
    observer <- sf::st_cast(observer, "POINT")
  }
  rm(valid_sf_types)

  # dsm_rast
  if (!is(dsm_rast, "SpatRaster")) {
    stop("dsm_rast needs to be a SpatRaster object!")
  } else if (sf::st_crs(terra::crs(dsm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dsm_rast needs to have the same CRS as observer")
  } else if (dsm_rast@pnt$res[1] != dsm_rast@pnt$res[2]) {
    stop("dsm_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }

  # dtm_rast
  if (!is(dtm_rast, "SpatRaster")) {
    stop("dtm_rast needs to be a SpatRaster object!")
  } else if (sf::st_crs(terra::crs(dtm_rast))$epsg != sf::st_crs(observer)$epsg) {
    stop("dtm_rast needs to have the same CRS as observer")
  }

  # max_distance
  max_distance <- round(max_distance, digits = 0)

  # raster_res
  dsm_res <- min(terra::res(dsm_rast))
  if (is.null(raster_res)) {
    raster_res = dsm_res
  } else if (raster_res < min(terra::res(dsm_rast))) {
    stop("raster_res must be higher than the resolution of dsm_rast")
  } else if ((raster_res %% dsm_res) != 0) {
    stop(paste0("raster_res must be a multible of the dsm_rast resolution. Try raster_res = ", raster_res - (raster_res %% dsm_res)))
  }
  rm(dsm_res)

  # spacing
  if (is.null(spacing)) {
    spacing <- raster_res
  }

  # folder_path
  if (!is.null(folder_path)) {
    if (!dir.exists(folder_path)) {
      dir.create(folder_path)
    }
    folder_path <- tempfile(pattern = "VVI_", tmpdir = file.path(folder_path),
                            fileext = ".gpkg")
  }
  # output_type
  output_type <- match.arg(output_type)

  # cores
  stopifnot(is.numeric(cores), cores > 0)
  if (.Platform$OS.type == "windows" && cores > 1) {
    cores = 1
    message(
      "Setting cores > 1 on a Windows OS is currently not supported.\\n",
      "The cores argument has been automatically set to 1")
  }

  # by_row
  stopifnot(is.logical(by_row))

  #### 2. Convert observer to points ####
  if (progress) {
    message("Preprocessing:")
    pb = txtProgressBar(min = 0, max = 5, initial = 0, style = 2)
  }

  if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("LINESTRING", "MULTILINESTRING")) {
    if (!by_row) {
      observer <- observer %>%
        sf::st_union() %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_line_sample(density = 1/spacing) %>%
        sf::st_cast("POINT") %>%
        sf::st_as_sf() %>%
        dplyr::rename(geom = x)
    } else {
      observer <- observer %>%
        dplyr::mutate(rowid = seq_len(dplyr::n())) %>%
        sf::st_union(by_feature = by_row) %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_line_sample(density = 1/spacing) %>%
        sf::st_cast("POINT") %>%
        sf::st_as_sf() %>%
        dplyr::rename(geom = x)
    }
  } else if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("POLYGON", "MULTIPOLYGON")) {
    if (!by_row) {
      points <- poly_to_points(obs = observer, dsm_rast = dsm_rast,
                                 spacing = spacing) %>%
        sf::st_as_sf() %>%
        dplyr::rename(geom = geometry)
      # join attributes back
      observer <- points %>%
        sf::st_join(observer %>%
                      dplyr::mutate(rowid = seq_len(dplyr::n())))

    } else {
      geom_name <- attr(observer, "sf_column")
      byrows <- observer %>%
        dplyr::mutate(rowid = seq_len(dplyr::n())) %>%
        split(., .$rowid)
      observer <- Reduce(rbind,
                         sapply(
                           byrows,
                           poly_to_points,
                           dsm_rast = dsm_rast,
                           spacing = spacing)
                         ) %>%
        sf::st_as_sf()
    }
  }
  if (progress) setTxtProgressBar(pb, 1)


  #### 3. Prepare data for viewshed analysis ####
  # Max AOI
  max_aoi <- observer %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_buffer(max_distance)

  # Crop DSM to max AOI and change resolution
  dsm_rast <- terra::crop(dsm_rast, terra::vect(max_aoi))

  if (raster_res != min(raster::res(dsm_rast))) {
    terra::terraOptions(progress = 0)
    dsm_rast <- terra::aggregate(dsm_rast, fact = raster_res/terra::res(dsm_rast))
    terra::terraOptions(progress = 3)
  }

  dsm_vec <- terra::values(dsm_rast, mat = FALSE)

  dsm_cpp_rast <- dsm_rast %>% terra::rast() %>% raster::raster()

  # Coordinates of start point
  x0 <- sf::st_coordinates(observer)[,1]
  y0 <- sf::st_coordinates(observer)[,2]

  # Observer heights
  height_0_vec <- unlist(terra::extract(dtm_rast, cbind(x0, y0)), use.names = F) + observer_height

  if (progress) setTxtProgressBar(pb, 2)


  #### 4. Remove points outside the DSM or DTM ####
  invalid_points <- unique(c(
    which(is.na(terra::extract(dsm_rast, cbind(x0, y0)))), # points outside the DSM
    which(is.na(height_0_vec)) # points outside the DTM
  ))

  # Remove invalid points
  if (length(invalid_points) > 0) {
    observer <- observer[-invalid_points, ]
    x0 <- x0[-invalid_points]
    y0 <- y0[-invalid_points]
    height_0_vec <- height_0_vec[-invalid_points]
  }

  if (progress) setTxtProgressBar(pb, 3)


  #### 5. Last steps of PreProcessing ####
  # Prepare observer for output
  observer <- observer %>%
    dplyr::mutate(VVI = as.numeric(NA),
                  id = 1:dplyr::n()) %>%
    dplyr::select(id, VVI, dplyr::everything())


  # convert x0/y0 to col/row
  c0 <- terra::colFromX(dsm_rast, x0)
  r0 <- terra::rowFromY(dsm_rast, y0)

  if (progress) setTxtProgressBar(pb, 4)



  # 6. Final update of Pre-Processing ProgressBar
  if (progress) {
    setTxtProgressBar(pb, 5)
    cat("\n")
  }
  if (length(invalid_points) == 1) {
    message("1 point has been removed, because it was outside of the DSM or DTM")
  } else if (length(invalid_points) > 1) {
    message(paste(length(invalid_points), "points have been removed, because they were outside of the DSM or DTM"))
  }
  invisible(gc())

  #### 7. Calculate viewsheds and VVI ####
  if (progress) {
    message(paste0("Computing VVI for ", nrow(observer), ifelse(nrow(observer)>1, " points:", " point:")))
    #pb = txtProgressBar(min = 0, max = length(observer_list), initial = 0, style = 3)
    start_time <- Sys.time()
  }

  if (progress) {
    on.exit({
      time_dif <- round(cores * ((as.numeric(difftime(Sys.time(), start_time, units = "s"))*1000) / nrow(observer)), 2)
      cat("\n")

      time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "m")))
      if (time_total < 1) {
        time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s")))

        if (time_total < 1) {
          time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s"))) * 1000
          message(paste("Total runtime:", time_total, " milliseconds"))
        } else {
          message(paste("Total runtime:", time_total, " seconds"))
        }
      } else {
        message(paste("Total runtime:", time_total, " minutes"))
      }

      message(paste("Average time for a single point:", time_dif, "milliseconds"))
    }, add = TRUE)
  }

  viewshed_indices <- VVI_cpp(dsm = dsm_cpp_rast, dsm_values = dsm_vec,
                          x0 = c0, y0 = r0, h0 = height_0_vec, radius = max_distance,
                          ncores = cores, display_progress = progress)

  valid_values <- unlist(lapply(viewshed_indices, is.numeric), use.names = FALSE)

  if (output_type == "VVI") {
    observer[valid_values,2] <- sapply(viewshed_indices[valid_values], length)

    # workaround; should rather have VVI_cpp return VVI directly instead of ncells_visible
    # this is probably an approximation and might be incorrect if viewshed is partly outside raster extent
    observer$VVI <- observer$VVI / (pi * (max_distance / raster_res)^2)

    if (!is.null(folder_path)) {
      sf::st_write(observer, folder_path, append = TRUE, quiet = T)
    }
    rm(dsm_cpp_rast, dsm_vec, c0, r0, height_0_vec)
    invisible(gc())
    return(observer)
  }

  if (output_type == "viewshed") {
    # summed viewshed
    summed_viewshed <- rast(dsm_rast)
    values(summed_viewshed) <- 0
    indices_count <- tabulate(unlist(viewshed_indices))
    indices_count <- indices_count[indices_count > 0]
    values(summed_viewshed)[sort(unique(unlist(viewshed_indices)))] <-
      indices_count
    rm(dsm_cpp_rast, dsm_vec, c0, r0, height_0_vec)
    invisible(gc())
    return(summed_viewshed)
  }

  if (output_type == "cumulative") {
    # cumulative VVI
    if (!by_row) {
      area_buffer <- observer %>%
        dplyr::filter(valid_values) %>%
        sf::st_geometry() %>%
        sf::st_buffer(max_distance) %>%
        sf::st_union() %>%
        sf::st_area()
      cumulative_vvi <- dplyr::n_distinct(unlist(viewshed_indices[valid_values])) /
        (as.numeric(area_buffer) / raster_res^2)
      rm(dsm_cpp_rast, dsm_vec, c0, r0, height_0_vec)
      invisible(gc())
      return(cumulative_vvi)
    } else {
      area_buffer <-
        Reduce(c, Map(sf::st_combine, split(observer, observer$rowid))) %>%
        sf::st_buffer(max_distance) %>%
        sf::st_area()
      nvisible <- vector(mode = "double", length = length(unique(observer$rowid[valid_values])))
      viewshed_indices_valid <- setNames(viewshed_indices[valid_values],
                                         observer$rowid[valid_values])
      for (i in unique(observer$rowid[valid_values])) {
        nvisible[i] <- dplyr::n_distinct(
          unlist(viewshed_indices_valid[observer$rowid[valid_values] == i])
          )
      }
      cumulative_vvi <- nvisible / (as.numeric(area_buffer) / raster_res^2)
      result <- data.frame(
        rowid = unique(observer$rowid[valid_values]),
        cvvi = cumulative_vvi
      )
      rm(dsm_cpp_rast, dsm_vec, c0, r0, height_0_vec)
      invisible(gc())
      return(result)
    }
  }
}



poly_to_points <- function(obs, dsm_rast, spacing) {
  observer_bbox <- sf::st_bbox(obs)
  vect_obs <- terra::vect(obs)
  values_obs <- terra::values(vect_obs)
  points <- terra::rast(xmin = observer_bbox[1], xmax = observer_bbox[3],
                     ymin = observer_bbox[2], ymax = observer_bbox[4],
                     crs = terra::crs(dsm_rast),
                     resolution = spacing, vals = 0) %>%
    terra::mask(vect_obs) %>%
    terra::as.points(values = FALSE)
  terra::values(points) <- values_obs[rep(seq_len(nrow(values_obs)),
                                          nrow(terra::geom(points))), ,
                                      drop = FALSE]
  return(points)
}
