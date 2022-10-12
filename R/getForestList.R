#' Defines list of 'forest' objects
#'
#' Defines list of 'forest' objects for a set of target locations
#'
#' @param pts target locations
#' @param fib forest imputation basis, returned by function buildForestImputationBasis
#' @param lct vector of land cover type
#' @param merge_trees boolean flag to simplify trees into diameter classes
#' @param correct_lidar boolean flag to correct density and mean height using information derived from lidar
#' @param dataset_path path to the 'Datasets' directory
#'
getForestList<-function(pts, fib, lct,
                        merge_trees = TRUE,
                        correct_lidar = TRUE,
                        dataset_path = "~/OneDrive/Datasets/") {

  message("  1. Reading MFE25")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)
  pts = sf::st_as_sf(pts)

  message("  2. Intersecting geometry to get forest composition and structure")
  pts_t = sf::st_transform(pts, sf::st_crs(mfe25_sf))
  a=sf::st_intersects(pts_t,sf::st_geometry(mfe25_sf))
  for_ws = sf::st_drop_geometry(mfe25_sf)[as.numeric(a),]
  rm(mfe25_sf)
  gc()

  #Build class string for ws cells
  message("  3. Building forest class strings for target locations")
  ws_cl1 = character(nrow(for_ws))
  ws_cl2 = character(nrow(for_ws))
  for_ws$C1 = as.numeric(cut(for_ws$o1,breaks = c(0,2,5,10), labels=0:2))
  for_ws$C2 = as.numeric(cut(for_ws$o2,breaks = c(0,2,5,10), labels=0:2))
  for_ws$C3 = as.numeric(cut(for_ws$o3,breaks = c(0,2,5,10), labels=0:2))
  for_ws$C1[is.na(for_ws$C1)] = 0
  for_ws$C2[is.na(for_ws$C2)] = 0
  for_ws$C3[is.na(for_ws$C3)] = 0
  for(i in 1:length(ws_cl1)) {
    s = ""
    if(for_ws$C1[i]>0) {
      s = paste0(s,for_ws$especie1[i],"_",for_ws$C1[i])
    }
    ws_cl1[i]=s
    if(ws_cl1[i]=="") ws_cl1[i] = "Desarbolado"
    if(for_ws$C2[i]>0) {
      s = paste0(s,"+",for_ws$especie2[i],"_",for_ws$C2[i])
    }
    ws_cl2[i]=s
    if(ws_cl2[i]=="") ws_cl2[i] = "Desarbolado"
  }

  #Assign to each pixel the code of the a IFN plot with the same class within 50 km
  #and with the closest elevation difference
  message("  4. Forest plot imputation")
  ifn_cl1 = fib$cl1
  ifn_cl2 = fib$cl2
  ifnelev = fib$elevation
  ifn_forestlist = fib$forest
  ifn_rocosidad = fib$rocosidad
  ifncc = sf::st_coordinates(sf::st_transform(sf::st_geometry(fib), sf::st_crs(pts)))

  cc = sf::st_coordinates(pts)
  wselev = pts$elevation
  cellifn = rep(NA,length(ws_cl1))
  # pb = txtProgressBar(0, length(ws_cl1), style = 3)
  notfound = 0

  for(i in 1:length(ws_cl1)) {
    # setTxtProgressBar(pb,i)
    if(lct[i]=="forest" || lct[i]=="wildland") {
      ifn_same_cl1 = which(ifn_cl1==ws_cl1[i])
      ifn_same_cl2 = which(ifn_cl2==ws_cl2[i])
      if(length(ifn_same_cl2)>0) {
        d=sqrt(rowSums(sweep(ifncc[ifn_same_cl2,, drop=FALSE],2,cc[i,])^2))
        close = d<50000
        n = names(d)
        if(length(d)==1) { #If only one cell assign this, regardless of distance and elevation difference
          cellifn[i] = n
        } else if(sum(close)==0) { #If none is closer than 50 km assign closest of the same class
          cellifn[i]= n[which.min(d)]
        } else { #Assign the one with smallest difference in elevation
          difele = abs(ifnelev[ifn_same_cl2]-wselev[i])[close]
          n =n[close]
          cellifn[i]= n[which.min(difele)]
        }
      } else if(length(ifn_same_cl1)>0) {
        d=sqrt(rowSums(sweep(ifncc[ifn_same_cl1,, drop=FALSE],2,cc[i,])^2))
        close = d<50000
        n = names(d)
        if(length(d)==1) { #If only one cell assign this, regardless of distance and elevation difference
          cellifn[i] = n
        } else if(sum(close)==0) { #If none is closer than 50 km assign closest of the same class
          cellifn[i]= n[which.min(d)]
        } else { #Assign the one with smallest difference in elevation
          difele = abs(ifnelev[ifn_same_cl1]-wselev[i])[close]
          n =n[close]
          cellifn[i]= n[which.min(difele)]
        }
      } else {
        notfound = notfound+1
      }
    }
  }

  if(notfound>0) message(paste0("[",notfound," forest cells out of ",sum(lct=="forest")," could not be imputated a forest plot]"))
  ws_forestlist = vector("list", length(cellifn))
  for(i in 1:length(ws_forestlist)) {
    if(!is.na(cellifn[i])) {
      f = ifn_forestlist[[as.numeric(cellifn[i])]]
      f$ID = as.numeric(cellifn[i])
      f$treeData = f$treeData[f$treeData$Height>0,1:6] # Treu arbres amb alçada 0 (tallats)
      if(ws_cl1[i]=="Desarbolado") f$treeData = f$treeData[numeric(0),]
      if(merge_trees) {
        if(nrow(f$treeData)>0) f <- medfate::forest_mergeTrees(f)
      }
    } else if(lct[i]=="wildland" || lct[i]=="forest") {
      f = medfate::emptyforest()
      f$ID = NA
    }
    ws_forestlist[[i]] = f
  }

  if(correct_lidar) {
    message("  5. Reading LiDAR data")
    bf=terra::rast(paste0(dataset_path,"Lidar/VariablesBiofisiques/RastersComplets/2016-2017/variables-biofisiques-arbrat-v1r0-bf-2016-2017.tif"))
    hm=terra::rast(paste0(dataset_path,"Lidar/VariablesBiofisiques/RastersComplets/2016-2017/variables-biofisiques-arbrat-v1r0-hmitjana-2016-2017.tif"))

    message("  6. Intersecting geometry with LiDAR to get foliar biomass and mean height")
    pts_t = terra::project(terra::vect(pts), terra::crs(bf))
    pts_bf =terra::extract(bf, pts_t)
    pts_hm =terra::extract(hm, pts_t)
    rm(bf)
    rm(hm)
    gc()

    message("  7. Correcting density and tree height")
    maxbasalarea = 120
    for(i in 1:length(ws_forestlist)) {
      f = ws_forestlist[[i]]
      nvec = f$treeData$N
      hvec = f$treeData$Height
      ntree = length(hvec)
      if(ntree>0) {
        w = f$ID
        if(!is.na(w)) {
          hm_ratio = pts_hm$`variables-biofisiques-arbrat-v1r0-hmitjana-2016-2017`[i]/fib$hm[w]
          bf_ratio = pts_bf$`variables-biofisiques-arbrat-v1r0-bf-2016-2017`[i]/fib$bf[w]
          if(!is.na(hm_ratio)) f$treeData$Height = hvec*hm_ratio
          if(!is.na(bf_ratio)) {
            ba = medfate::stand_basalArea(f)
            if((ba*bf_ratio) > maxbasalarea) bf_ratio = maxbasalarea/ba
            f$treeData$N = nvec*bf_ratio
          }
          ws_forestlist[[i]] = f
        }
      }
    }
  }
  return(ws_forestlist)
}
