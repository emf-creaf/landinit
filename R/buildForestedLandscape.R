#' Builds an object of SpatialPixelsLandscape for a target area
#'
#' @param boundaries spatial polygons delimiting the target area
#' @param grid raster definition to resample topography at the desired resolution
#' @param dataset_path path to the 'Datasets' directory
#'
buildForestedLandscape<-function(boundaries, grid,
                                 dataset_path = "~/OneDrive/Datasets/") {
  message("A. TOPOGRAPHY")
  sgt_pnasm <-landinit::buildTopography(boundaries, grid = grid,
                                        dataset_path = dataset_path)
  gc()
  pts <- sf::st_as_sf(terra::as.points(sgt_pnasm))
  rm(sgt_pnasm)
  gc()

  message("B. LAND COVER")
  lct <- landinit::getLandCoverType(pts, dataset_path = dataset_path)
  gc()

  pts_forest <- pts[lct=="forest",]
  lct_forest <- lct[lct=="forest"]
  rm(pts)

  message("C. SOIL LIST")
  soil_list <- landinit::getSoilGridsParams(pts_forest, dataset_path = dataset_path)

  message("D. FOREST IMPUTATION BASIS")
  fib <- landinit::buildForestImputationBasis(dataset_path = dataset_path)

  message("E. FOREST LIST")
  forest_list <- landinit::getForestList(pts_forest, fib, lct_forest, dataset_path = dataset_path)


  message("F. BUILD SpatialPixelsLandscape")
  spdf <- as(pts_forest,"Spatial")
  gridded(spdf)<-TRUE
  spt <- SpatialPixelsTopography(spdf@coords, elevation = spdf$elevation,
                                 slope = spdf$slope, aspect = spdf$aspect,
                                 proj4string = spdf@proj4string,
                                 grid = spdf@grid)
  spl <- SpatialPixelsLandscape(spt, lct, forest_list, soil_list)
  return(spl)
}
