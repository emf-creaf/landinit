#' @export
buildSoils<-function(pts, dataset_path = "~/OneDrive/Datasets/", widths = c(300,700,1000,2000)) {

  soilgrids_points_var<-function(pts, var = "bdod", dataset_path = "~/OneDrive/Datasets/") {
    m <- c(terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_0_5.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_5_15.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_15_30.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_30_60.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_60_100.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_100_200.tif")))
    x <- terra::project(pts, terra::crs(m))
    y<-terra::extract(m, x)[,-1]
    names(y) = c("0_5","5_15","15_30","30_60","60_100","100_200")
    return(y)
  }

  # Bulk density
  bd = soilgrids_points_var(pts, var = "bdod")
  bd = bd/100
  #Clay
  clay = soilgrids_points_var(pts, var = "clay")
  clay = clay/10

  #Sand
  sand = soilgrids_points_var(pts, var = "sand")
  sand = sand/10

  #rock fragment content
  rfc = soilgrids_points_var(pts, var = "cfvo")
  rfc = rfc/10

  #Organic matter
  om = soilgrids_points_var(pts, var = "soc")
  om = om/100

  sg_widths = c(50,100,150,300,400,1000)
  n <- length(pts)
  soil_list <- vector("list", n)
  pb = txtProgressBar(1, n, style = 3)
  for(i in 1:n) {
    setTxtProgressBar(pb, i)
    df <- data.frame(widths = sg_widths,
                     clay = as.numeric(clay[i,]),
                     sand = as.numeric(sand[i,]),
                     om = as.numeric(om[i,]),
                     bd = as.numeric(bd[i,]),
                     rfc = as.numeric(rfc[i,]))
    soil_list[[i]] <- medfate::redefineSoilLayers(df, widths)
  }


  #Soil depth
  m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDRICM_M_250m_ll.tif"))
  bdricm <-terra::extract(m, terra::project(pts, terra::crs(m)))[,-1]
  bdricm <- bdricm*10 #cm to mm

  #Prob of occurence of R horizon
  m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDRLOG_M_250m_ll.tif"))
  bdrlog <-terra::extract(m, terra::project(pts, terra::crs(m)))[,-1]
  bdrlog <- bdrlog/100 #prob of occurence of R horizon

  #Absolute depth
  m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDTICM_M_250m_ll.tif"))
  bdticm<-terra::extract(m, terra::project(pts, terra::crs(m)))[,-1]
  bdticm <- bdticm*10 #cm to mm


  #Modification of soil depths according to Shagguan
  oridepths = c(300,1000,2000,4000)
  for(i in 1:length(soil_list)) {
    soilparams = soil_list[[i]]
    absolutesoildepth = bdticm[i]
    Rhorizondepth = bdricm[i]
    if(!is.na(Rhorizondepth)) {
      ## If Rhorizon is < 1 m trim soil depth
      if(Rhorizondepth<oridepths[2]) {
        soilparams = soilparams[c(1:2,4),]
        soilparams$widths[2] = Rhorizondepth - oridepths[1]
        soilparams$widths[3] = 4000 - sum(soilparams$widths[1:2])
        soilparams$rfc[3] = 95
      } else if(Rhorizondepth<oridepths[3]) {
        soilparams = soilparams[c(1:3,4),]
        soilparams$widths[3] = Rhorizondepth - oridepths[2]
        soilparams$widths[4] = 4000 - sum(soilparams$widths[1:3])
        soilparams$rfc[4] = 95
      }
    }
    soil_list[[i]] = soilparams #Replace data
  }

  return(soil_list)
}


