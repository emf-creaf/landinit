addLandCoverType<-function(pts = NULL,
                           dataset_path = "~/OneDrive/Datasets/"){

  message("1. Reading MFE25")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)
  mfe25_sf = mfe25_sf[,c("tipo_estru","usomfe")]

  pts = sf::st_as_sf(pts)
  pts_t = sf::st_transform(pts, sf::st_crs(mfe25_sf))

  mfe25data = sf::st_drop_geometry(mfe25_sf)
  mfe25geom = sf::st_geometry(mfe25_sf)

  message("2. Intersecting geometry")
  a=sf::st_intersects(pts_t, mfe25geom)

  message("3. Reclassifying land cover")
  for_ws = mfe25data[as.numeric(a),]
  tipostru = for_ws$tipo_estru
  usomfe = for_ws$usomfe
  pts$lct = rep("static", length(usomfe))
  pts$lct[usomfe %in% c("Agua")] = "water"
  pts$lct[usomfe %in% c("Artificial")] = "artificial"
  pts$lct[usomfe %in% c("Arbolado", "Arbolado disperso", "Arbolado ralo", "Desarbolado")] = "wildland"
  pts$lct[usomfe %in% c("Cultivos")] = "agriculture"
  pts$lct[tipostru %in% c("Afloramientos rocosos")] = "rock"

  return(pts)
}
