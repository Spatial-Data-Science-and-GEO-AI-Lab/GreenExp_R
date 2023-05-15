#' @title Visualize Weights
#' @description Helper function to visualize the parameters used in the \code{\link[GreenExp]{vgvi_from_sf}} function.
#'
#' @param x numeric or object of class \code{\link[terra]{rast}}; Either numeric, indicating the buffer distance or a \code{\link[terra]{rast}} of the \code{\link[GreenExp]{viewshed}} function.
#' @param m numeric; See ‘Details’.
#' @param b numeric; See ‘Details’.
#' @param mode character; 'logit' or 'exponential'. See ‘Details’.
#'
#' @details The type of function, used for calculating the distance decay weights, can be defined with the \code{mode} parameter.
#' The argument 'logit' uses the logistic function, d = 1 / (1 + e^(b * (x - m))) and 'exponential' the exponential function d = 1 / (1 + (b * x^m)).
#' The decay function can be visualized using the \code{\link[GreenExp]{viewshed}} function.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom sf st_coordinates
#' @importFrom terra xyFromCell
visualizeWeights <- function(x, m = 0.5, b = 8, mode = c("logit", "exponential")) {
  if (is(x, "SpatRaster")) {
    # Get XY coordinates that are visible
    xy <- x %>%
      terra::xyFromCell(which(x[] == 1))

    # Calculate maximum distance
    max_dist = (terra::nrow(x)/2) * terra::res(x)[1]
  } else if (is.numeric(x)) {
    max_dist <- x
  } else {
    stop("x needs to be numeric or a SpatRaster object")
  }

  if (mode == c("logit", "exponential") || mode == "logit") {
    logfun <- function(x){
      return(1 / (1 + exp((b) * (x - m))))
    }

    plot_main <- paste0("Mode: logit\nm: ", m, "    b: ", b)
  } else if(mode == "exponential") {
    logfun <- function(x){
      return(1/(1 + ((b) * x^(m))))
    }
    plot_main <- paste0("Mode: exponential\nm: ", m, "    b: ", b)
  } else {
    stop("Currently only logit and exponential are supported")
  }

  plot(logfun(seq(0, 1, length.out = max_dist)), type = "l",
       ylab = "Decay Weight (d)", xlab = "Distance [m]",
       main = plot_main)
}
