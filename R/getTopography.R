#' Extracts a raster with topography variables from DEM
#'
#' Extracts a raster with elevation, slope and aspect for a target area
#'
#' @param boundaries spatial object (normally polygons) delimiting the target area
#' @param grid raster definition to resample topography at the desired resolution
#' @param mask boolean flag to mask raster according to boundaries
#' @param dataset_path path to the 'Datasets' directory
#'
getTopography<-function(boundaries, grid = NULL,
                        mask = TRUE,
                        dataset_path = "~/OneDrive/Datasets/") {
  #Load topography
  message("  1. Load DEM")
  sgt_terra = c(terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_elevation_30m.tif")),
                terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_slope_30m.tif")),
                terra::rast(paste0(dataset_path,"MDT/Products/Catalunya_aspect_30m.tif")))
  names(sgt_terra)<-c("elevation", "slope", "aspect")

  # If grid is not missing resample topography
  message("  2. Crop DEM (after possibly resampling grid)")
  if(!is.null(grid)) {
    terra::crs(grid) <- terra::crs(sgt_terra)
    sgt_terra <- terra::resample(sgt_terra, grid, method="bilinear")
  }
  gc()

  # Crop raster to the vector data provided
  sgt_out <-terra::crop(sgt_terra, terra::vect(boundaries), mask = mask)
  gc()
  return(sgt_out)
}
