#' Builds an object of SpatialPixelsLandscape for a target area
#'
#' This function makes internal calls to other package functions to obtain topography, land cover,
#' soil and forest data over the target area.
#'
#' @param boundaries spatial polygons delimiting the target area
#' @param grid raster definition to resample topography at the desired resolution
#' @param fib forest imputation basis, returned by function buildForestImputationBasis
#' @param dataset_path path to the 'Datasets' directory
#'
#' @seealso buildForestImputationBasis
#'
buildForestedLandscape<-function(boundaries, grid, fib,
                                 dataset_path = "~/OneDrive/Datasets/") {
  message("A. TOPOGRAPHY")
  sgt_pnasm <-landinit::getTopography(boundaries, grid = grid,
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

  message("D. FOREST LIST")
  forest_list <- landinit::getForestList(pts_forest, fib, lct_forest, dataset_path = dataset_path)

  message("E. MODIFY SOIL ROCK CONTENT")
  for(i in 1:length(soil_list)) {
    f = forest_list[[i]]
    if(!is.na(f$ID)) {
      roc = c(5.0, 12.5, 37.5, 67.5, 87.5)[fib$rocosidad[f$ID]]
    } else {
      roc = 5.0
    }
    soil_list[[i]] = medfateutils::modifySoilRockContent(soil_list[[i]], roc)
  }

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
