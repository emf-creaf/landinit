getForestList<-function(pts, fib, lct,
                        dataset_path = "~/OneDrive/Datasets/") {

  message("1. Reading MFE25")
  mfe25_sf <- sf::st_read(paste0(dataset_path,"MFE/Sources/MFE25/mfe_Catalunia.shp"),
                          quiet = TRUE)

  pts = sf::st_as_sf(pts)
  pts_t = sf::st_transform(pts, sf::st_crs(mfe25_sf))

  message("2. Intersecting geometry")
  a=sf::st_intersects(pts_t,sf::st_geometry(mfe25_sf))
  for_ws = sf::st_drop_geometry(mfe25_sf)[as.numeric(a),]
  rm(mfe25_sf)
  gc()

  #Build class string for ws cells
  message("3. Building class strings for target locations")
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
    # if(for_ifn$C3[i]>0) {
    #   s = paste0(s,"+",for_ifn$SP3[i],"_",for_ifn$C3[i])
    # }
  }

  #Assign to each pixel the code of the a IFN plot with the same class within 50 km
  #and with the closest elevation difference
  message("4. Forest plot imputation")
  ifn_cl1 = fib$ifn_cl1
  ifn_cl2 = fib$ifn_cl2
  sp_ifn_sf = fib$sp_ifn_sf
  ifnelev = fib$ifnelev
  ifn_forestlist = fib$ifn_forestlist

  ifncc = sf::st_coordinates(sf::st_transform(sp_ifn_sf, sf::st_crs(pts)))
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
      f$treeData = f$treeData[f$treeData$Height>0,] # Treu arbres amb alçada 0 (tallats)
      if(ws_cl1[i]=="Desarbolado") f$treeData = f$treeData[numeric(0),]
      ws_forestlist[[i]] = f
    } else if(lct[i]=="wildland" || lct[i]=="forest") {
      ws_forestlist[[i]] = medfate::emptyforest()
    }
  }
  return(ws_forestlist)
}
