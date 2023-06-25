#' Viewshed Greenness Visibility Index (VGVI) from sf
#' @description The VGVI expresses the proportion of visible greenness to the total visible area based on a \code{\link[GreenExp]{viewshed}}.
#' The estimated VGVI values range between 0 and 1, where 0 = no green cells are visible, and 1 = all of the visible cells are green.
#' A distance decay function is applied, to account for the reducing visual prominence of an object in space with increasing distance from the observer.
#'
#' @param observer object of class \code{sf}; Observer location(s) from where the VGVI should be computed. See ‘Details’ for valid sf geometry types
#' @param dsm_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DSM
#' @param dem_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the DEM
#' @param greenspace_rast object of class \code{\link[terra]{rast}}; \code{\link[terra]{rast}} of the binary Greenspace mask. Values of the Greenspace mask must be 1 for Green and 0 for No-Green
#' @param max_distance numeric; Buffer distance to calculate the viewshed
#' @param observer_height numeric > 0; Height of the observer (e.g. 1.7 meters)
#' @param raster_res optional; NULL or numeric > 0; Resolution that the GVI raster should be aggregated to. Needs to be a multible of the dsm_rast resolution
#' @param spacing optional; numeric > 0; Only if \code{observer} is a linestring (or polygon), points on the line (or on a grid) will be generated.
#' The \code{spacing} parameter sets the distance in between the points on the line/grid
#' @param m numeric; See ‘Details’
#' @param b numeric; See ‘Details’
#' @param mode character; 'logit' or 'exponential'. See ‘Details’
#' @param cores numeric; The number of cores to use, i.e. at most how many child processes will be run simultaneously
#' @param folder_path optional; Folder path to where the output should be saved continuously. Must not inklude a filename extension (e.g. '.shp', '.gpkg').
#' @param progress logical; Show progress bar and computation time?
#'
#' @examples
#'# Load a sf dataframe
#' df <- sf::st_read('path/to/data')
#'
#' # load the raster files
#' DEM<- terra::rast('path/to/dem')
#' DSM <- terra::rast('path/to/dsm')
#' GS <- terra::rast('path/to/greenspace')
#'
#' # Calculate the vgvi
#' vgvi_from_sf(df, dsm_rast = DSM, dem_rast = DEM, greenspace_rast = GS,
#'                   mode = "logit")
#'
#'
#' @details
#' observer needs to be a geometry of type POINT, LINESTRING, MULTILINESTRING, POLYGON or MULTIPOLYGON. If observer is a LINESTRING or MULTILINESTRING,
#' points will be generated along the line(s) every "resolution" meters. If observer is a POLYGON or MULTIPOLYGON, a grid with resolution = "resolution"
#' will be generated, and VGVI will be computed for every point.
#' The CRS (\code{\link[sf]{st_crs}}) needs to have a metric unit!
#'
#' The type of function, used for calculating the distance decay weights, can be defined with the \code{mode} parameter.
#' The argument 'logit' uses the logistic function, d = 1 / (1 + e^(b * (x - m))) and 'exponential' the exponential function d = 1 / (1 + (b * x^m)).
#'
#'
#'
#'
#' @return sf_object containing the weighted VGVI values as POINT features, where 0 = no green cells are visible, and 1 = all of the visible cells are green.
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

vgvi_from_sf <- function(observer, dsm_rast, dem_rast, greenspace_rast,
                         max_distance = 800, observer_height = 1.7,
                         raster_res = NULL, spacing = raster_res,
                         m = 0.5, b = 8, mode = c("logit", "exponential"),
                         cores = 1, folder_path = NULL, progress = FALSE) {

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
  }  else if(sf::st_crs(terra::crs(dsm_rast))$epsg != sf::st_crs(observer)$epsg){
    warning('The crs of your raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_dsm_rast <- sf::st_crs(observer)$epsg
    dsm_rast <- terra::project(dsm_rast, paste0('EPSG:',epsg_dsm_rast))

  } else if(dsm_rast@ptr$res[1] != dsm_rast@ptr$res[2]) {
    stop("dsm_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
  }

  # dem_rast
  if (!is(dem_rast, "SpatRaster")) {
    stop("dem_rast needs to be a SpatRaster object!")
  } else if(sf::st_crs(terra::crs(dem_rast))$epsg != sf::st_crs(observer)$epsg){
    warning('The crs of your DEM raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_dem_rast <- sf::st_crs(observer)$epsg
    dem_rast <- terra::project(dem_rast, paste0('EPSG:',epsg_dem_rast))

  }

  # greenspace_rast
  if (!is(greenspace_rast, "SpatRaster")) {
    stop("greenspace_rast needs to be a SpatRaster object!")
  } else if(sf::st_crs(terra::crs(greenspace_rast))$epsg != sf::st_crs(observer)$epsg){
    warning('The crs of your greenspace raster is not the same as the projected crs of the input location,
              the projected crs of the raster will be transformed into the projected crs of the address location')
    epsg_greenspace_rast <- sf::st_crs(observer)$epsg
    greenspace_rast <- terra::project(greenspace_rast, paste0('EPSG:',epsg_greenspace_rast))

  } else if(dsm_rast@ptr$res[1] != dsm_rast@ptr$res[2]) {
    stop("greenspace_rast: x and y resolution must be equal.\nSee https://github.com/STBrinkmann/GVI/issues/1")
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

  # mode
  if (is.character(mode) && (mode == "logit")){
    mode = 1
  } else if (is.character(mode) && (mode == "exponential")){
    mode = 2
  } else if (is.character(mode) && (mode == c("logit", "exponential"))){
    mode = 2
  } else {
    stop("mode must be logit or exponential")
  }

  # folder_path
  if (!is.null(folder_path)) {
    if (!dir.exists(folder_path)) {
      dir.create(folder_path)
    }
    folder_path <- tempfile(pattern = "VGVI_", tmpdir = file.path(folder_path), fileext = ".gpkg")
  }


  #### 2. Convert observer to points ####
  if(progress) {
    message("Preprocessing:")
    pb = txtProgressBar(min = 0, max = 5, initial = 0, style = 2)
  }

  if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("LINESTRING", "MULTILINESTRING")) {
    observer <- observer %>%
      sf::st_union() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_line_sample(density = 1/spacing) %>%
      sf::st_cast("POINT") %>%
      sf::st_as_sf() %>%
      dplyr::rename(geom = x)
  } else if (as.character(sf::st_geometry_type(observer, by_geometry = FALSE)) %in% c("POLYGON", "MULTIPOLYGON")) {
    observer_bbox <- sf::st_bbox(observer)
    observer <- terra::rast(xmin = observer_bbox[1], xmax = observer_bbox[3],
                            ymin = observer_bbox[2], ymax = observer_bbox[4],
                            crs = terra::crs(dsm_rast), resolution = spacing, vals = 0) %>%
      terra::crop(terra::vect(observer)) %>%
      terra::mask(terra::vect(observer)) %>%
      terra::xyFromCell(which(terra::values(.) == 0)) %>%
      as.data.frame() %>%
      sf::st_as_sf(coords = c("x","y"), crs = sf::st_crs(observer)) %>%
      dplyr::rename(geom = geometry)
    rm(observer_bbox)
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
  greenspace_rast <- terra::crop(greenspace_rast, terra::vect(max_aoi))

  if(raster_res != min(raster::res(dsm_rast))) {
    terra::terraOptions(progress = 0)
    dsm_rast <- terra::aggregate(dsm_rast, fact = raster_res/terra::res(dsm_rast))
    terra::terraOptions(progress = 3)
  }

  dsm_vec <- terra::values(dsm_rast, mat = FALSE)
  greenspace_vec <- terra::values(greenspace_rast, mat = FALSE)

  dsm_cpp_rast <- dsm_rast %>% terra::rast() %>% raster::raster()
  greenspace_cpp_rast <- greenspace_rast %>% terra::rast() %>% raster::raster()


  # Coordinates of start point
  x0 <- sf::st_coordinates(observer)[,1]
  y0 <- sf::st_coordinates(observer)[,2]

  # Observer heights
  height_0_vec <- unlist(terra::extract(dem_rast, cbind(x0, y0)), use.names = F) + observer_height

  if (progress) setTxtProgressBar(pb, 2)


  #### 4. Remove points outside the DSM or DEM ####
  invalid_points <- unique(c(
    which(is.na(terra::extract(dsm_rast, cbind(x0, y0)))), # points outside the DSM
    which(is.na(height_0_vec)) # points outside the dem
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
    dplyr::mutate(VGVI = as.numeric(NA),
                  id = 1:dplyr::n()) %>%
    dplyr::select(id, VGVI, dplyr::everything())


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
    message("1 point has been removed, because it was outside of the DSM or DEM")
  } else if (length(invalid_points) > 1) {
    message(paste(length(invalid_points), "points have been removed, because they were outside of the DSM or DEM"))
  }
  invisible(gc())

  #### 7. Calculate viewsheds and VGVI ####
  if (progress) {
    message(paste0("Computing VGVI for ", nrow(observer), ifelse(nrow(observer)>1, " points:", " point:")))
    #pb = txtProgressBar(min = 0, max = length(observer_list), initial = 0, style = 3)
    start_time <- Sys.time()
  }

  vgvi_values <- VGVI_cpp(dsm = dsm_cpp_rast, dsm_values = dsm_vec,
                          greenspace = greenspace_cpp_rast, greenspace_values = greenspace_vec,
                          x0 = c0, y0 = r0, radius = max_distance, h0 = height_0_vec,
                          fun = mode, m = m, b = b, ncores = cores, display_progress = progress)

  valid_values <- unlist(lapply(vgvi_values, is.numeric), use.names = FALSE)
  observer[valid_values,2] <- vgvi_values[valid_values]

  if (!is.null(folder_path)) {
    sf::st_write(observer, folder_path, append = TRUE, quiet = T)
  }


  if (progress) {
    time_dif <- round(cores * ((as.numeric(difftime(Sys.time(), start_time, units = "s"))*1000) / nrow(observer)), 2)
    cat("\n")

    time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "m")))
    if(time_total < 1){
      time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s")))

      if(time_total < 1){
        time_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "s")))*1000
        message(paste("Total runtime:", time_total, " milliseconds"))
      } else {
        message(paste("Total runtime:", time_total, " seconds"))
      }
    } else {
      message(paste("Total runtime:", time_total, " minutes"))
    }

    message(paste("Average time for a single point:", time_dif, "milliseconds"))
  }

  rm(dsm_cpp_rast, dsm_vec, greenspace_cpp_rast, greenspace_vec, c0, r0, height_0_vec)
  invisible(gc())
  message('This code is retrieved from the GVI function, for more information look at https://github.com/STBrinkmann/GVI ')

  return(observer)
}
