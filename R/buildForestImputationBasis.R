#' Assembles forest plot data for forest stand imputation
#'
#' @param dataset_path path to the 'Datasets' directory
#' @param ifn_source string indicating the IFN survey (either 'IFN3' or 'IFN4') to use as imputation basis
#'
buildForestImputationBasis<-function(dataset_path = "~/OneDrive/Datasets/",
                                     ifn_source = "IFN4",
                                     additional_sources = character(0)) {
  message("  1. Reading MFE25 and LiDAR data")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)

  bf=terra::rast(paste0(dataset_path,"Lidar/VariablesBiofisiques/RastersComplets/2016-2017/variables-biofisiques-arbrat-v1r0-bf-2016-2017.tif"))
  hm=terra::rast(paste0(dataset_path,"Lidar/VariablesBiofisiques/RastersComplets/2016-2017/variables-biofisiques-arbrat-v1r0-hmitjana-2016-2017.tif"))

  message("  2. Reading forest plot data")
  if(ifn_source == "IFN3") {
    ifn_forestlist = readRDS(paste0(dataset_path,"IFN/Products/IFN3/Rdata/forestlist_roots_IFN3_Catalunya.rds"))
    spdf_ifn_topo = readRDS(paste0(dataset_path,"IFN/Products/Topography/IFN23_spt_cat_unique_ETRS89H31.rds"))
    ifn_plotdata = readRDS(paste0(dataset_path,"IFN/Products/IFN3/Rdata/plotDataIFN3_Catalunya.rds"))
    # Subset forest list to plots with available coordinates
    spdf_ifn_topo = spdf_ifn_topo[row.names(spdf_ifn_topo@data) %in% names(ifn_forestlist)]
    ids = row.names(spdf_ifn_topo@coords)
    ifn_forestlist = ifn_forestlist[ids]
    rocosidad = as.numeric(ifn_plotdata[ids,"ROCOSID"])
    ifn_sf = sf::st_transform(sf::st_as_sf(as(spdf_ifn_topo,"SpatialPointsDataFrame")),
                              sf::st_crs(mfe25_sf))
    ifn_sf$rocosidad = rocosidad
  } else if(ifn_source == "IFN4") {
    ifn_forestlist = readRDS(paste0(dataset_path,"IFN/Products/IFN4/Rdata/forestlist_roots_IFN4_Catalunya.rds"))
    spdf_ifn_topo = readRDS(paste0(dataset_path,"IFN/Products/Topography/IFN4_spt_cat_ETRS89H31.rds"))
    ifn_plotdata = readRDS(paste0(dataset_path,"IFN/Products/IFN4/Rdata/plotDataIFN4_Catalunya.rds"))
    # Subset forest list to plots with available coordinates
    spdf_ifn_topo = spdf_ifn_topo[row.names(spdf_ifn_topo@data) %in% names(ifn_forestlist)]
    ids = row.names(spdf_ifn_topo@coords)
    ifn_forestlist = ifn_forestlist[ids]
    rocosidad = as.numeric(ifn_plotdata[ids,"Rocosid"])
    ifn_sf = sf::st_transform(sf::st_as_sf(as(spdf_ifn_topo,"SpatialPointsDataFrame")),
                              sf::st_crs(mfe25_sf))
    ifn_sf$rocosidad = rocosidad
  }
  if("PNASM3" %in% additional_sources) {
    pnasm_forestlist = readRDS(paste0(dataset_path, "ParquesNacionales/PNASM/Products/PNASM3/forestlistPNASM3.rds"))
    spdf_pnasm_topo = readRDS(paste0(dataset_path,"ParquesNacionales/PNASM/Products/Topography/spt_PNASM_ETRS89H31N.rds"))
    pnasm_plotdata = readRDS(paste0(dataset_path,"ParquesNacionales/PNASM/Products/PNASM3/plotDataPNASM3.rds"))
    # Subset forest list to plots with available coordinates
    spdf_pnasm_topo = spdf_pnasm_topo[row.names(spdf_pnasm_topo@data) %in% names(pnasm_forestlist)]
    ids = row.names(spdf_pnasm_topo@coords)
    pnasm_forestlist = pnasm_forestlist[ids]
    pnasm_sf = sf::st_transform(sf::st_as_sf(as(spdf_pnasm_topo,"SpatialPointsDataFrame")),
                                sf::st_crs(mfe25_sf))
    pnasm_sf$rocosidad = rep(NA, length(ids))
    ifn_sf = rbind(ifn_sf, pnasm_sf)
    ifn_forestlist = c(ifn_forestlist, pnasm_forestlist)
  }

  #Determine MFE of IFN cells
  message("  3. Intersecting geometry with MFE25 to get forest composition and structure")
  a=sf::st_intersects(ifn_sf, sf::st_geometry(mfe25_sf))
  for_ifn = sf::st_drop_geometry(mfe25_sf)[as.numeric(a),]
  rm(mfe25_sf)
  gc()

  message("  4. Intersecting geometry with LiDAR to get foliar biomass and mean height")
  ifn_vec = terra::vect(sf::st_geometry(ifn_sf))
  ifn_vec = terra::project(ifn_vec, terra::crs(bf))
  ifn_bf =terra::extract(bf, ifn_vec)
  ifn_hm =terra::extract(hm, ifn_vec)
  rm(bf)
  rm(hm)
  gc()

  #Build class string for IFN cells
  message("  5. Build class strings for forest plots")
  ifn_cl1 = character(nrow(for_ifn))
  ifn_cl2 = character(nrow(for_ifn))
  for_ifn$C1 = as.numeric(cut(for_ifn$o1,breaks = c(0,2,5,10), labels=0:2))
  for_ifn$C2 = as.numeric(cut(for_ifn$o2,breaks = c(0,2,5,10), labels=0:2))
  for_ifn$C3 = as.numeric(cut(for_ifn$o3,breaks = c(0,2,5,10), labels=0:2))
  for_ifn$C1[is.na(for_ifn$C1)] = 0
  for_ifn$C2[is.na(for_ifn$C2)] = 0
  for_ifn$C3[is.na(for_ifn$C3)] = 0
  for(i in 1:length(ifn_cl1)) {
    s = ""
    if(for_ifn$C1[i]>0) {
      s = paste0(s,for_ifn$especie1[i],"_",for_ifn$C1[i])
    }
    ifn_cl1[i]=s
    if(ifn_cl1[i]=="") ifn_cl1[i] = "Desarbolado"
    if(for_ifn$C2[i]>0) {
      s = paste0(s,"+",for_ifn$especie2[i],"_",for_ifn$C2[i])
    }
    ifn_cl2[i]=s
    if(ifn_cl2[i]=="") ifn_cl2[i] = "Desarbolado"
  }
  ifn_sf$cl1 = ifn_cl1
  ifn_sf$cl2 = ifn_cl2
  ifn_sf$bf = ifn_bf$`variables-biofisiques-arbrat-v1r0-bf-2016-2017`
  ifn_sf$hm = ifn_hm$`variables-biofisiques-arbrat-v1r0-hmitjana-2016-2017`
  ifn_sf$forest = ifn_forestlist
  return(ifn_sf)
}
