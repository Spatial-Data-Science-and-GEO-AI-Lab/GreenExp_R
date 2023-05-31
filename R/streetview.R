#access_token <- "MLY|5946359745468210|66b4730cad535105b96da4fac2cf73f3"

streetview <- function(address_location, access_token, x_dist=0.0025, y_dist=0.0025){

  projected_crs <- sf::st_crs(address_location)
  address_location <- sf::st_transform(address_location, 4326)
   # create your access token at https://mapillary.com/developer
  metadata_endpoint <- "https://graph.mapillary.com"

  coords <- sf::st_coordinates(address_location)
  coords <- data.frame(coords)
  headers <- c("Authorization" = paste("OAuth", access_token))

  for (i in 1:nrow(coords)){
    #create a bbox for each coordinate
    url_imagesearch[i] <- paste0(metadata_endpoint, "/images?fields=id&bbox=",
                              coords[i,1] - x_dist, ",", coords[i,2] - y_dist, ",",
                              coords[i,1] + x_dist, ",", coords[i,2] + y_dist)
  }

  responses <- lapply(url_imagesearch, function(url) {
    response <- httr::GET(url, httr::add_headers(headers))
    return(response)
  })


  data_imagesearch <- lapply(responses, function(url){
    images <- httr::content(url, as = "parsed")
  })

  # Print the number of images found
  cat("Images found:", length(data_imagesearch$data), "\n")

  # Create empty lists
  ids <- list()
  urls <- list()
  list_indices <- list()

  for (i in seq_along(data_imagesearch)) {
    image_list <- data_imagesearch[[i]]
    for (image in image_list$data) {
      # Image details
      url_image <- paste0(metadata_endpoint, "/", image$id, "?fields=id,thumb_2048_url,captured_at,sequence")
      response_image <- httr::GET(url_image, add_headers(headers))
      data_image <- httr::content(response_image, as = "parsed")

      # Store the ID and URL in the corresponding lists
      ids <- c(ids, data_image$id)
      urls <- c(urls, data_image$thumb_2048_url)
      list_indices <- c(list_indices, i)
    }
  }

  # Create a new data frame
    a <- cbind(ids, urls, list_indices)
  new_df <- data.frame(a)

  return(new_df)


}

