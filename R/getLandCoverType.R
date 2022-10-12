#' Retrieves the land cover type of a target geometry
#'
#' @param pts target points
#' @param dataset_path path to the 'Datasets' directory
#' @param separate_forests boolean flag to separate forests from shrublands/grasslands
#'
getLandCoverType<-function(pts = NULL,
                           dataset_path = "~/OneDrive/Datasets/",
                           separate_forests = TRUE){

  message("  1. Reading MFE25")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)
  mfe25_sf = mfe25_sf[,c("tipo_estru","usomfe")]

  pts = sf::st_as_sf(pts)
  pts_t = sf::st_transform(pts, sf::st_crs(mfe25_sf))

  mfe25data = sf::st_drop_geometry(mfe25_sf)
  mfe25geom = sf::st_geometry(mfe25_sf)

  message("  2. Intersecting geometry")
  a=sf::st_intersects(pts_t, mfe25geom)

  message("  3. Reclassifying land cover")
  for_ws = mfe25data[as.numeric(a),]
  tipostru = for_ws$tipo_estru
  usomfe = for_ws$usomfe
  lct = rep("static", length(usomfe))
  lct[usomfe %in% c("Agua")] = "water"
  lct[usomfe %in% c("Artificial")] = "artificial"
  if(separate_forests) {
    lct[usomfe %in% c("Arbolado", "Arbolado disperso", "Arbolado ralo")] = "forest"
    lct[usomfe %in% c("Desarbolado")] = "shrubland/grassland"
  } else {
    lct[usomfe %in% c("Arbolado", "Arbolado disperso", "Arbolado ralo", "Desarbolado")] = "wildland"
  }
  lct[usomfe %in% c("Cultivos")] = "agriculture"
  lct[tipostru %in% c("Afloramientos rocosos")] = "rock"

  return(lct)
}
