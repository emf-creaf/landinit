#' Builds a list of soil objects for a set of target point locations
#'
#' @param pts target locations
#' @param widths soil layer widths (in mm)
#' @param modify_soil_depth truncates soil depth according to Shagguan et al. (2017)
#' @param dataset_path path to the 'Datasets' directory
#'
getSoilGridsParams<-function(pts,
                             widths = c(300,700,1000,2000),
                             modify_soil_depth = TRUE,
                             dataset_path = "~/OneDrive/Datasets/") {

  soilgrids_points_var<-function(pts_terra, var = "bdod") {
    m <- c(terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_0_5.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_5_15.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_15_30.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_30_60.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_60_100.tif")),
           terra::rast(paste0(dataset_path, "Soils/Sources/SoilGrids/",toupper(var),"/",var,"_100_200.tif")))
    x <- terra::project(pts_terra, terra::crs(m))
    y<-terra::extract(m, x)[,-1]
    names(y) = c("0_5","5_15","15_30","30_60","60_100","100_200")
    return(y)
  }

  message("  1. Extracting data from SoilGrids")
  # Coerce to terra vector
  pts_terra <- terra::vect(pts)

  # Bulk density
  bd = soilgrids_points_var(pts_terra, var = "bdod")
  bd = bd/100
  #Clay
  clay = soilgrids_points_var(pts_terra, var = "clay")
  clay = clay/10

  #Sand
  sand = soilgrids_points_var(pts_terra, var = "sand")
  sand = sand/10

  #rock fragment content
  rfc = soilgrids_points_var(pts_terra, var = "cfvo")
  rfc = rfc/10

  #Organic matter
  om = soilgrids_points_var(pts_terra, var = "soc")
  om = om/100


  #Soil depth
  if(modify_soil_depth) {
    m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDRICM_M_250m_ll.tif"))
    bdricm <-terra::extract(m, terra::project(pts_terra, terra::crs(m)))[,-1]
    bdricm <- bdricm*10 #cm to mm
  }

  message("  2. Defining soil parameter data frames")
  sg_widths = c(50,100,150,300,400,1000)
  n <- length(pts_terra)
  soil_list <- vector("list", n)
  # pb = txtProgressBar(1, n, style = 3)
  for(i in 1:n) {
    # setTxtProgressBar(pb, i)
    df <- data.frame(widths = sg_widths,
                     clay = as.numeric(clay[i,]),
                     sand = as.numeric(sand[i,]),
                     om = as.numeric(om[i,]),
                     bd = as.numeric(bd[i,]),
                     rfc = as.numeric(rfc[i,]))
    df_redef <- medfateutils::redefineSoilLayers(df, widths)
    if(modify_soil_depth) soil_list[[i]] <- medfateutils::modifySoilDepth(df_redef, bdricm[i])
    else soil_list[[i]] <- df_redef
  }
#
#
#   #Prob of occurence of R horizon
#   m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDRLOG_M_250m_ll.tif"))
#   bdrlog <-terra::extract(m, terra::project(pts, terra::crs(m)))[,-1]
#   bdrlog <- bdrlog/100 #prob of occurence of R horizon
#
#   #Absolute depth
#   m<-terra::rast(paste0(dataset_path, "Soils/Sources/SoilDepth_Shangguan2017/BDTICM_M_250m_ll.tif"))
#   bdticm<-terra::extract(m, terra::project(pts, terra::crs(m)))[,-1]
#   bdticm <- bdticm*10 #cm to mm

  return(soil_list)
}


