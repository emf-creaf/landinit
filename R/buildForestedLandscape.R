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
  sgt_out <-landinit::getTopography(boundaries, grid = grid,
                                        dataset_path = dataset_path)
  gc()
  pts <- sf::st_as_sf(terra::as.points(sgt_out))
  rm(sgt_out)
  gc()

  message("B. LAND COVER")
  lct <- landinit::getLandCoverType(pts, dataset_path = dataset_path)
  gc()

  pts_forest <- pts[lct=="forest",]
  lct_forest <- lct[lct=="forest"]
  rm(pts)

  message("C. SOIL DATAFRAME LIST")
  soil_dataframe_list <- landinit::getSoilGridsParams(pts_forest, dataset_path = dataset_path)

  message("D. FOREST LIST")
  forest_list <- landinit::getForestList(pts_forest, fib, lct_forest, dataset_path = dataset_path)

  message("E. MODIFY SOIL ROCK CONTENT")
  #Looks for the closest forest plot and get its content
  ifncc = sf::st_coordinates(sf::st_transform(sf::st_geometry(fib), sf::st_crs(pts_forest)))
  cc = sf::st_coordinates(pts_forest)
  for(i in 1:length(soil_dataframe_list)) {
    d=sqrt(rowSums(sweep(ifncc,2,cc[i,])^2))
    ind = which.min(d)
    roc_class = fib$rocosidad[ind]
    if(!is.na(roc_class)) {
      roc = c(5.0, 12.5, 37.5, 67.5, 87.5)[roc_class]
    } else {
      roc = 5.0
    }
    soil_dataframe_list[[i]] = medfateutils::modifySoilRockContent(soil_dataframe_list[[i]], roc)
  }

  message("F. BUILD SpatialPixelsLandscape")
  spdf <- as(pts_forest,"Spatial")
  gridded(spdf)<-TRUE
  spt <- SpatialPixelsTopography(spdf@coords, elevation = spdf$elevation,
                                 slope = spdf$slope, aspect = spdf$aspect,
                                 proj4string = spdf@proj4string,
                                 grid = spdf@grid)
  spl <- SpatialPixelsLandscape(spt, lct, forest_list, soil_dataframe_list)
  return(spl)
}
