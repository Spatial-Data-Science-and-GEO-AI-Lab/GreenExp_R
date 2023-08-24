
#' Green Coverage Street
#'
#' @param city A city or place name as string to set the bounding are of the city to extract OSM streets
#' @param buffer_distance A distance in meters to create a buffer around each street segment to defined an area within which the green coverage will be assess. Usually smaller values preferred (e.g., 30, 50)
#' @param folder_path_land_cover Folder path to where the retrieved land cover should be saved continuously. Must not include a filename extension.
#' @param epsg_code Optional; a  epsg code to get a Projected CRS in the final output, If missing, the default is `3395`
#' @param UID Optional; a  character string representing a unique identifier for each point street segment
#' @param year Optional; The year of the satellite images. The years 2020 and 2021 can be used for the time being.
#' @param plot_landcover Optional; to plot the land cover, default is `FALSE`
#'
#' @examples
#'
#' GreenStcover <- green_cover_streets ("Centrum, Amsterdam", buffer_distance = 30)
#'
#' #map green coverage that includes tree, grass, shrubs together
#' mapview::mapview(GreenStcover, zcol= "greencover")
#'
#' #map the tree coverage
#' mapview::mapview(GreenStcover, zcol= "tree_cover")
#'
#'
#' #plot the result in tmap if mapview does not work!
#' tmap::tmap_mode("view")
#' tmap::tm_shape(GreenStcover) + tmap::tm_lines (col = "greencover", palette = "viridis", basemaps = "Esri.WorldTopoMap")
#'
#'
#' tmap::tm_shape(GreenStcover) + tmap::tm_lines (col = "tree_cover", palette = "viridis", basemaps = "Esri.WorldTopoMap")
#'
#'
#' @return The percentage of each green cover and individual vegetation cover type within a given buffer each street.
#' @export
#' @importFrom tmaptools geocode_OSM
#' @importFrom sfnetworks as_sfnetwork
#' @importFrom sfnetworks to_spatial_smooth
#' @importFrom sfnetworks activate
#' @importFrom tidygraph convert
#'
#'
#'
#'


green_cover_streets <- function(city=NULL, buffer_distance=NULL,
                                folder_path_land_cover = NULL, epsg_code=NULL,  UID=NULL,
                                year='2021', plot_landcover=FALSE) {

  ###### 1. Preparation + Cleaning #######

  library(magrittr)
  start_function <- Sys.time()

  projected_crs<-sf::st_crs(epsg_code)

  boundbox <- tmaptools::geocode_OSM(city, as.sf = T,  geometry = c("bbox"))

  lines <- osmextract::oe_get(city, stringsAsFactors=FALSE, boundary = boundbox,
                              max_file_size = 5e+09, boundary_type = 'spat')


  #clean the lines before analysis using sfnetworks and tidygraph
  net = sfnetworks::as_sfnetwork(lines) #create a network file

  #now do the cleaning for edges that has loop or multiple
  simple = net %>%
    sfnetworks::activate("edges") %>%
    filter(!edge_is_multiple()) %>%
    filter(!edge_is_loop())

  #smooth the simplified network
  library(sfnetworks)
  smoothed = tidygraph::convert(simple, to_spatial_smooth)

  #get the line file back agai
  lines <- smoothed %>% sfnetworks::activate("edges") %>% sf::st_as_sf() %>% dplyr::select(-".tidygraph_edge_index")

  #lines <- sf::st_intersection(lines, boundbox)


  lines$ID <- 1:nrow(lines)

  calculation_area <- sf::st_buffer(lines, dist = buffer_distance)


  ###### 3. Raster for land cover #######
  if (!is.character(year)){
    year <- is.character(year)
  }

  address <- lines
  address <- sf::st_transform(address, 4326)

  calculation_area <- sf::st_transform(calculation_area, 4326)
  bbox <- sf::st_bbox(address)

  # Search in the planetary computer and get the esa world cover collection
  matches <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
    rstac::stac_search(collections = "esa-worldcover",
                       bbox = bbox) %>%
    rstac::get_request() %>%   rstac::items_sign(sign_fn = rstac::sign_planetary_computer())
  # Look for the start date times in the esa-worldcover collection
  date_time <- matches %>%
    rstac::items_reap(field = c("properties", "start_datetime"))


  # get the years in planetary computer
  years <- substr(date_time, 1, 4)



  if (year %in% years) {
    year <- as.integer(year)
    date_object <- lubridate::make_datetime(year)
    # Format the POSIXct object to the desired format
    date <- format(date_object, format = "%Y-%m-%dT%H:%M:%SZ")

  } else{
    warning('The sattelite do not have images in the year selected by you,
             it will be set to default year, which is 2021.')
    # Also make variable year to 2021 for the message.
    year <- 2021
    date_object <- lubridate::make_datetime(year)
    # Format the POSIXct object to the desired format
    date <- format(date_object, format = "%Y-%m-%dT%H:%M:%SZ")
  }

  # Select the year whcih is put in by the user, otherwise the default year.
  selected_item <- matches$features[[which(date_time == date)]]

  # # Select the year which is put in by the user, otherwise the default year.
  # selected_item <- matches$features[[which(date_time == paste0(year,"-01-01T00:00:00Z"))]]

  cat('The data is retrieved from the', selected_item$properties$description,
      '\n The product is based on', selected_item$properties$mission,
      '\n The data is retrieved in the year', year, '\n')
  land_cover <- terra::rast( paste0("/vsicurl/", selected_item$assets$map$href))
  bbox_proj <- bbox %>%  sf::st_as_sfc() %>%  sf::st_transform(sf::st_crs(land_cover)) %>% terra::vect()
  land_cover <- terra::rast( paste0("/vsicurl/", selected_item$assets$map$href)) %>%  terra::crop(bbox_proj)

  if (!is.null(folder_path_land_cover)) {
    if (!dir.exists(folder_path_land_cover)) {
      dir.create(folder_path_land_cover)
    }
    f <- file.path(paste0(folder_path_land_cover,'/','land_cover.tif'))
    terra::writeRaster(land_cover, f, overwrite=T )
  }

  if (plot_landcover){


    df <- as.data.frame(land_cover, xy = TRUE)
    names(df) <- c('x','y','land_cover')

    df$land_cover <- as.character(df$land_cover)

    df <- df %>%
      dplyr::mutate(land_cover = dplyr::case_when(
        land_cover == "10" ~ "treecover",
        land_cover == "20" ~ "shrubland",
        land_cover == "30" ~ "grassland",
        land_cover == "40" ~ "cropland",
        land_cover == "50" ~ "built-up",
        land_cover == "60" ~ "bare_vegetation",
        land_cover == "70" ~ "snow/ice",
        land_cover == "80" ~ "perm_water_bodies",
        land_cover == "90" ~ "herb_wetland",
        land_cover == "95" ~ "mangroves",
        land_cover == "100" ~ "moss/lichen",
        # Add more conditions as needed
        TRUE ~ land_cover  # Keep the original value if no condition matches
      ))


    plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y, fill = land_cover)) +
      ggplot2::geom_tile() +
      ggplot2::theme_bw()


    legend_title <- "Land Cover"
    legend_labels <- c("treecover" = "#006400", "shrubland"="#FFBB22",
                       "grassland"="#FFFF4C", "cropland"="#F096FF", "built-up"= "#FA0000", "bare_vegetation"="#B4B4B4",
                       "snow/ice"="#F0F0F0", "perm_water_bodies"="#0064C8", "herb_wetland"="#0096A0",
                       "mangroves"="#00CF75", "moss/lichen"="#FAE6A0" )
    plot <- plot + ggplot2::scale_fill_manual(values=legend_labels)
    print(plot)


  }
  # Check if crs is the same
  calculation_area_proj <- sf::st_transform(calculation_area, terra::crs(land_cover))
  # extract the values
  land_cover_values <- terra::extract(land_cover, calculation_area_proj)
  names(land_cover_values) <- c('ID', 'land_cover')

  raster_values <- replace(land_cover_values, is.na(land_cover_values), 0)
  # count the amount of land cover values per polygon
  class_raster_values <- dplyr::count(raster_values, ID, N = get('land_cover'))
  # Convert to wide format + replacing NA,this operation gives error if there is only one land cover type found within the area
  class_raster_values_wide <- tidygraph::mutate_all(tidyr::spread(class_raster_values, N, n), ~tidyr::replace_na(.,0))
  # Calculate the %
  class_raster_values_perc <- round(cbind(class_raster_values_wide$ID, class_raster_values_wide[,-1]/rowSums(class_raster_values_wide[,-1])), 2)
  codes <- c("UID", unique(c('10', '20', '30', '40', '50', '60', '70', '80', '90', '95', '100')))
  # replace the NA values to <NA>
  codes <- replace(codes, is.na(codes), "<NA>")

  names(class_raster_values_perc)[1] <- "UID"
  if (!missing(UID)){
    class_raster_values_perc$UID <- UID
  }

  missings <- setdiff(c("UID", codes), names(class_raster_values_perc))
  missings_df <- setNames(data.frame(matrix(ncol = length(missings), nrow = nrow(lines))), missings)
  missings_df <- replace(missings_df, is.na(missings_df), 0)
  df <- data.frame(class_raster_values_perc, missings_df)
  #order the columns to give meaningful names to the columns
  df <- df[(c("UID", "X10", "X20",
              "X30", "X40", "X50", "X60", "X70",
              "X80", "X90", "X95", "X100"))]
  # Give the columns meaningful names
  names(df) <- c("UID","tree_cover", "shrubland",
                 "grassland", "cropland", "built-up", "bare_vegetation", "snow_ice",
                 "perm_water_bodies", "herbaceous_wetland", "mangroves", "moss_lichen")

  #Only keep columns that are present in analysis
  empty_cols <- colSums(is.na(df)) == nrow(df)

  # Subset the dataframe to exclude empty columns
  df <- df[, !empty_cols]
  df <- data.frame(df, address) %>% sf::st_as_sf() %>%
    dplyr::mutate(greencover = (tree_cover + grassland + shrubland)*100) %>%
    dplyr::select(osm_id, UID, greencover, tree_cover, grassland, shrubland, cropland)


  return(df)

}
