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
  sgt_cat <- readRDS(paste0(dataset_path,"MDT/Products/Catalunya30m_SpatialGridTopography.rds"))
  sgt_cat <- as(sgt_cat, "SpatialGridDataFrame")
  sgt_terra = terra::rast(raster::brick(sgt_cat)) # Conversion to terra
  rm(sgt_cat)
  gc()

  # If grid is not missing resample topography
  if(!is.null(grid)) {
    terra::crs(grid) <- terra::crs(sgt_terra)
    sgt_terra <- terra::resample(sgt_terra, grid, method="bilinear")
  }

  # Crop raster to the vector data provided
  sgt_out <-terra::crop(sgt_terra, terra::vect(boundaries), mask = TRUE)
  gc()
  return(sgt_out)
}
