access_token <- "MLY|5946359745468210|66b4730cad535105b96da4fac2cf73f3"

streetview <- function(address_location, access_token, x_dist=0.00025, y_dist=0.00025){

  projected_crs <- sf::st_crs(address_location)
  address_location <- sf::st_transform(address_location, 4326)
   # create your access token at https://mapillary.com/developer
  metadata_endpoint <- "https://graph.mapillary.com"

  coords <- sf::st_coordinates(address_location)
  coords <- data.frame(coords)
  headers <- c("Authorization" = paste("OAuth", access_token))
  url_imagesearch <- list()

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

  # Print the number of i), "\n")

  # Create empty lists
  ids <- list()
  urls <- list()
  list_indices <- list()
  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                   total = length(data_imagesearch),
                                   complete = "=",   # Completion bar character
                                   incomplete = "-", # Incomplete bar character
                                   current = ">",    # Current bar character
                                   clear = FALSE,    # If TRUE, clears the bar when finish
                                   width = 100)      # Width of the progress bar


  for (i in seq_along(data_imagesearch)) {
    image_list <- data_imagesearch[[i]]
    for (image in image_list$data) {
      pb$tick
      # Image details
      url_image <- paste0(metadata_endpoint, "/", image$id, "?fields=id,thumb_2048_url,captured_at,sequence")
      response_image <- httr::GET(url_image, httr::add_headers(headers))
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

#######
# function extension potential

# address <- sf::st_read('data/Ams_Test_multiple_home_locations1000.gpkg')
# access_token <- "MLY|5946359745468210|66b4730cad535105b96da4fac2cf73f3"
#
# a <- address[1:3,]
#
# df <- streetview(a, access_token = access_token)
# library(reticulate)
# library(here)
# reticulate::py_config()
# transformers <- reticulate::import("transformers")
# pil          <- import('PIL')
# requests     <- import('requests')
# torch        <- import('torch')
# numpy <- import('numpy')
#
# url <- paste0(df$urls[1],'.jpg')
#
# image <- pil$Image$open(requests$get(url, stream=T)$raw)
#
#
# #
# #
# # processor <- transformers$AutoImageProcessor$from_pretrained("facebook/mask2former-swin-large-cityscapes-semantic")
# # model <- transformers$Mask2FormerForUniversalSegmentation$from_pretrained("facebook/mask2former-swin-large-cityscapes-semantic")
# #
# # with(torch$no_grad(),
# #      outputs<-model(inputs['pixel_values'], return_dict=TRUE))
# #
# #
# # class_queries_logits <- outputs$class_queries_logits
# # mask_queries_logits <- outputs$masks_queries_logits$detach()
#
#
#
# processor = transformers$SegformerFeatureExtractor$from_pretrained("nvidia/segformer-b5-finetuned-cityscapes-1024-1024")
# model = transformers$SegformerForSemanticSegmentation$from_pretrained("nvidia/segformer-b5-finetuned-cityscapes-1024-1024")
#
#
#
# inputs <- processor(images=image, return_tensors="pt")
# # outputs = model(inputs['pixel_values'], return_dict=TRUE)
# # token_logits = model(inputs['pixel_values'], return_dict=TRUE)$logits
#
# with(torch$no_grad(),
#      outputs<-model(inputs['pixel_values'], return_dict=TRUE))
#
# logits = outputs$logits$cpu()
#
#
# logits_np <- numpy$asarray(logits)
#
#
# # Get the predicted label as a scalar
#
# logits<-outputs$logits
#
# unsampled_logits <- torch$nn$functional$interpolate(logits,
#                                                     size=(c(image$size[2], image$size[1])),
#                                                     mode='bilinear',
#                                                     align_corners = FALSE)
#
# predicted_mask = unsampled_logits$argmax(dim=as.integer(1))$cpu()$numpy()
#
#
# PALETTE <- list(
#   c(128, 64, 128), c(244, 35, 232), c(70, 70, 70), c(102, 102, 156),
#   c(190, 153, 153), c(153, 153, 153), c(250, 170, 30), c(220, 220, 0),
#   c(107, 142, 35), c(152, 251, 152), c(70, 130, 180), c(220, 20, 60),
#   c(255, 0, 0), c(0, 0, 142), c(0, 0, 70), c(0, 60, 100), c(0, 80, 100),
#   c(0, 0, 230), c(119, 11, 32)
# )
#
# color_map <- lapply(seq_along(PALETTE), function(i) { i - 1 })
# names(color_map) <- lapply(seq_along(PALETTE), function(i) { PALETTE[[i]] })
#
#
# vis <- numpy$zeros(dim(predicted_mask)[1:2], 3)
#
# vis <- numpy$zeros(predicted_mask$shape)
# # Convert upsampled_logits tensor to a NumPy array
# prediction <- torch$argmax(unsample_logits, dim = as.integer(1))$cpu()$numpy()
#
# # Create an array of zeros with the shape of prediction
# for (i in seq_along(color_map)) {
#   c <- color_map[[i]]
#   vis[predicted_mask == i] <- as.character(c)
# }
#
#
# mask <- pil$Image$fromarray(vis$astype(numpy$uint8))
#
# vis <-  numpy$astype(vis, numpy$uint8)
#
# vis <- numpy$uint8(vis)
# mask <- pil$Image$fromarray(vis)

