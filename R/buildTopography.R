#' Extracts a raster with topography variables from DEM
#'
#' Extracts a raster with elevation, slope and aspect for a target area
#'
#' @param boundaries spatial polygons delimiting the target area
#' @param grid raster definition to resample topography at the desired resolution
#' @param dataset_path path to the 'Datasets' directory
#'
buildTopography<-function(boundaries, grid = NULL,
                          dataset_path = "~/OneDrive/Datasets/") {
  #Load topography
  sgt_terra = c(terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_elevation_30m.tif")),
                terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_slope_30m.tif")),
                terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_aspect_30m.tif")))
  names(sgt_terra)<-c("elevation", "slope", "aspect")

  # If grid is not missing resample topography
  if(!is.null(grid)) {
    terra::crs(grid) <- terra::crs(sgt_terra)
    sgt_terra <- terra::resample(sgt_terra, grid, method="bilinear")
  }
  gc()

  # Crop raster to the vector data provided
  sgt_out <-terra::crop(sgt_terra, terra::vect(boundaries), mask = TRUE)
  gc()
  return(sgt_out)
}
