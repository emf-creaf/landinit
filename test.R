library(landinit)

dataset_path = "~/OneDrive/Datasets/"

#Grid topology at 100 m resolution
xmin=256100
ymin=4488100
nrows=2720
ncols=2640
xmax = xmin + nrows*100
ymax = ymin + ncols*100
x100 <- terra::rast(nrows = nrows, ncols = ncols, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
                 resolution = c(100,100))

nrows=2720/2
ncols=2640/2
xmax = xmin + nrows*200
ymax = ymin + ncols*200
x200 <- terra::rast(nrows = nrows, ncols = ncols, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
                    resolution = c(200,200))

# Load PNASM limits (zona periferica)
ppnn = sf::st_read("~/OneDrive/Datasets/ProtectedAreas/Spain/ParquesNacionales/ParquesNacionales_P_B.shp")
pnasm = ppnn[1,]
pnasm_perif = sf::st_read(paste0(dataset_path, "ParquesNacionales/PNASM/Sources/Limits/ZonaPeriferica_AIGUESTORTES.shp"))

pnasm <- sf::st_transform(pnasm, sf::st_crs(pnasm_perif))
boundaries <-sf::st_union(pnasm, pnasm_perif, by_feature = TRUE)

sgt_pnasm <-landinit::buildTopography(boundaries, grid = x200)
gc()

v <- sf::st_as_sf(terra::as.points(sgt_pnasm))

v2 <- landinit::addLandCoverType(v)
gc()

v3 <- landinit::addSoilGridsParams(v2)
