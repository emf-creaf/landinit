#' Assembles forest plot data for forest stand imputation
#'
#' @param dataset_path path to the 'Datasets' directory
#'
buildForestImputationBasis<-function(dataset_path = "~/OneDrive/Datasets/") {
  message("1. Reading MFE25")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)

  message("2. Reading forest plot data")
  ifn_forestlist = readRDS(paste0(dataset_path,"IFN/Products/IFN3/Rdata/forestlist_roots_IFN3_Catalunya.rds"))

  spdf_ifn_topo = readRDS(paste0(dataset_path,"IFN/Products/Topography/IFN23_spt_cat_unique_ETRS89H31.rds"))
  spdf_ifn_topo = spdf_ifn_topo[row.names(spdf_ifn_topo@data) %in% names(ifn_forestlist)]
  ids = row.names(spdf_ifn_topo@coords)

  # Subset forest list to plots with available coordinates
  ifn_forestlist = ifn_forestlist[ids]

  ifnelev = spdf_ifn_topo@data$elevation
  sp_ifn = as(spdf_ifn_topo,"SpatialPoints")

  sp_ifn_sf = sf::st_transform(sf::st_as_sf(sp_ifn), sf::st_crs(mfe25_sf))



  #Determine MFE of IFN cells
  message("3. Intersecting geometry")
  a=sf::st_intersects(sp_ifn_sf,sf::st_geometry(mfe25_sf))
  for_ifn = sf::st_drop_geometry(mfe25_sf)[as.numeric(a),]
  row.names(for_ifn)<-row.names(spdf_ifn_topo@data)
  rm(mfe25_sf)
  gc()

  #Build class string for IFN cells
  message("4. Build class strings for forest plots")
  ifn_cl1 = character(nrow(for_ifn))
  ifn_cl2 = character(nrow(for_ifn))
  names(ifn_cl1)<-row.names(spdf_ifn_topo@data)
  names(ifn_cl2)<-row.names(spdf_ifn_topo@data)
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
    # if(for_ifn$C3[i]>0) {
    #   s = paste0(s,"+",for_ifn$SP3[i],"_",for_ifn$C3[i])
    # }
  }

  return(list(sp_ifn_sf = sp_ifn_sf, ifn_cl1 = ifn_cl1, ifn_cl2 = ifn_cl2,
              ifnelev = ifnelev, ifn_forestlist = ifn_forestlist))
}
