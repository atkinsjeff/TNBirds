# TN Bird test run Monroe County data

require(lidR)
require(viridis)
require(sp)
las.files <- list.files("./data_las/", full.names = TRUE, pattern = "*.las")

# read in test
las <- readLAS(las.files[13])
las <- sf::st_transform(las, sf::st_crs(32616))

# LASfile <- system.file("extdata", "example.laz", package="rlas")
# las = readLAS(LASfile)
# st_crs(las)$Name
# st_bbox(las)
tlas <- sf::st_transform(las, sf::st_crs(32616))
st_crs(tlas)$Name
st_bbox(tlas)


# convert to meters
ft2m = 1200/3937
# las$X = las$X * ft2m
# las$Y = las$Y * ft2m
las$Z = las$Z * ft2m
# what is needed is to convert from feet to meters
# is that done better in the conversion in arc pro?

dtm <- lidR::grid_terrain(las, 10, algorithm = tin())

x11()
plot(dtm, col = viridis(32))

# normalize
las.norm <- lidR::normalize_height(las, dtm, na.rm = TRUE)

# clip circle
roi <- lidR::clip_circle(las.norm, xcenter = 777000, ycenter = 3932000, radius = 50)


# roi <- lidR::filter_poi(roi, Z > 0, Z < 125)
plot(roi)

require(g)

# chm
chm <- lidR::grid_canopy(las.norm, res = 10, algorithm = p2r())

x11()
plot(chm, col = viridis(32))



# las <- readLAS(clean.list.of.files[1])

# custom function to work with lidR::grid_metrics()
# needs better documentation.......obviously
# z = Z, height, rn = ReturnNumber, i = Intensity, a = ScanAngle
myMetrics <- function(z, rn, i, a){
  first  = rn == 1L
  zfirst = z[first]
  nfirst = length(zfirst)
  nz = length(z)


  metrics = list(
    MeanAngle = mean(a),
    MedAngle = median(a),
    MaxAngle = max(a),
    NoPoints = length(z),
    iqr =  IQR(z), # inter-quartile range
    vci = VCI(z, zmax = max(z)), # vertical complexity index
    entropy = entropy(z, zmax = max(z)),
    fhd =   (entropy(z, zmax = max(z)) * log(max(z))),  #foliar height diversity
    vdr = ( (max(z) - median(z)) / max(z)),
    top_rug = sd(zfirst),
    moch = mean(zfirst),
    skew = (3 * (mean(z) - median(z)) / sd(z)), # Skew = 3 * (Mean – Median) / Standard Deviation
    firstbelow1 = (sum(zfirst < 1) / nfirst) * 100,
    firstbelow2 = (sum(zfirst < 2) / nfirst) * 100,
    firstbelow5 = (sum(zfirst < 5) / nfirst) * 100,
    # total returns
    below1 = (sum(z < 1) / nz) * 100,
    below2 = (sum(z < 2) / nz) * 100,
    below5 = (sum(z < 5) / nz) * 100,
    p10 = quantile(z, probs = 0.1),
    p25 = quantile(z, probs = 0.25),
    p75 = quantile(z, probs = 0.75),
    p90 = quantile(z, probs = 0.9),
    p95 = quantile(z, probs = 0.95),
    isd = sd(i),
    imean = mean(i))
  #NoEdge = sum(edge))
  # return all them boys
  return(metrics)
}


# for loop that iterates through files from index file and calculates grid metrics then
# saves each raster stack as a .tif file
for(i in 1:length(clean.list.of.files)){



  # read all the las files in
  las <- lidR::readLAS(clean.list.of.files[i], filter="-drop_overlap -drop_class 17")

  # dtm at 5 m resolution
  # dtm <- lidR::grid_terrain(las, 1, algorithm = knnidw(k = 6L, p = 2))
  dtm <- lidR::grid_terrain(las, 1, algorithm = tin())

  # change max global.....this is if you are running this local, also if you do this with
  # readLAScatalog() you need to set a temp folder or your life will be hell
  # options(future.globals.maxSize= 891289600)

  # normalize heights
  las.norm <- lidR::normalize_height(las, dtm, na.rm = TRUE)

  # remove big ol' files to clear memory
  rm(dtm)
  rm(las)

  # removing negatives and crazy hight values, in meters. This step is CRUCIAL
  las.norm <- filter_poi(las.norm, Z > 0, Z <= 40)   # should be written to REPLACE 0, not remove. Maybe just great than 0

  # if you prefer base lidR standard metrics use the commented out code
  # metrics <- grid_metrics(las, ~metrics_custom(z=Z), 10) # calculate grid metrics

  # custom metrics function, note input variables and resolution, which is set at 10 as in 10 meters
  metrics <- lidR::grid_metrics(las.norm, ~myMetrics(z = Z, rn=ReturnNumber, i = Intensity, a = ScanAngle), res = 10 )    #df <- raster::as.data.frame(metrics, xy=TRUE, na.rm = TRUE)

  # # this renames them
  stack.names = c("MeanAngle", "MedAngle", "MaxAngle", "NoPoints", "IQR", "VCI", "Entropy", "FHD", "VDR", "Top Rugosity", "MOCH", "Skewness", "FirstBelow1m", "FirstBelow2m", "FirstBelow5m",
                  "Below1m", "Below2m", "Below5m", "p10", "p25", "p75", "p90", "p95", "SDIntensity", "MeanIntensity")

  names(metrics) <- stack.names

  # Before we save the files, we need to name them
  # make sure to edit the value in there. also, overwrite is set to TRUE
  # so if you don't chnage that, you will write over your data
  # out file name for SGL 316
  out.file.name <- paste("/nfs/forestbirds-data/processed/gamelands_lidar_metrics/gamelands_316_metrics", substr(basename(clean.list.of.files[i]), 1, nchar(basename(clean.list.of.files[i])) - 4), "_custom_metrics.tif", sep = "")

  # writes metrics file raster stack to disk
  writeRaster(metrics, out.file.name, format = "GTiff", overwrite = TRUE)

  # removing to save memory
  rm(las.norm)

  # this snippet let's you know how many more .laz files are left to process and what time it is
  message("how many las files left")
  print(length(clean.list.of.files) - i)
  print(Sys.time())

}
