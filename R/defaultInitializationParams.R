#' Default initialization params
#'
#' Creates a list of default parameters for landscape initialization procedures
#'
#' @return A list with the following elements:
#'  \itemize{
#'    \item{correct_lidar: boolean}
#'  }
#'
defaultInitializationParams<-function() {
  l = list(merge_trees = TRUE,
       correct_lidar = TRUE,
       modify_soil_depth = TRUE,
       rfc_estimation = "constant")
  return(l)
}
