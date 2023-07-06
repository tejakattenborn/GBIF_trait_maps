library(raster)
library(data.table)
library(terra)
#Load Data

dtn = 30
setDTthreads(threads = dtn)

#define PFT:
#pft = c("Shrub", "Tree", "Grass")
#pft = c("Shrub", "Tree")
pft = c("Grass")

file <- paste0("/net/home/tkattenborn/data_global_maps/sPlotOpen_TRYgapfilled_cwm", paste0(pft, collapse = "_"), ".csv") #random sample max. 10000 obs, hex9, GBIF, TRY-gapfilled

data <- fread(file)
#data <- read.csv(file)

head(data)
xy <- cbind(data$Longitude, data$Latitude)

# raster for a 2 degree resolution map
r02 <- raster(ncols = 1800, nrows = 900)
r05 <- raster(ncols = 720, nrows = 360)
r2 <- raster(ncols = 180, nrows = 90)

rasters <- c(r02, r05, r2)
#rasters <- c(r05)
loop.vector <- which(grepl("X", colnames(data))) #15:47 # loop over these trait columns in dataframe
#loop.vector <- 15:15 # loop over these trait columns in dataframe
folder_name <- c("02deg","05deg","2deg")
#folder_name <- c("05deg")
#folder_name <- c("05deg")

index <- 1
for (j in rasters) {
  
  for (i in loop.vector) { # Loop over loop.vector
    
    data_sub <- data[,i, with=FALSE]
    data_sub[[1]] <- as.numeric(data_sub[[1]])
    xy_sub <- xy
    
    #remove outliers
    q = quantile(data_sub[[1]], probs = c(0.02, 0.98))
    too_small <- which(data_sub[[1]] < q[1])
    data_sub <- data_sub[-too_small,1]
    xy_sub <- xy_sub[-too_small,]
    too_large <- which(data_sub[[1]] > q[2])
    data_sub <- data_sub[-too_large,1]
    xy_sub <- xy_sub[-too_large,]
    
    vals <- data_sub[[1]]
    name1 <- colnames(data[,i, with=FALSE])
    #r1 <- rasterize(xy, j, vals, fun = mean)
    r1 <- rasterize(xy_sub, j, as.numeric(vals), fun=function(x,...)c(length(x),mean(x),median(x),sd(x), quantile(x, probs = c(0.05)), quantile(x, probs = c(0.95))))
    r1[is.infinite(r1)] <- NA
    crs(r1) <- "+proj=longlat"
    
    names(r1)<-c("count", "mean", "median", "sd", "quantile05", "quantile95")
    #export as GeoTiff -->  separate file for each trait
    
    filename1 = paste("/net/home/tkattenborn/data_global_maps/trait_maps_gbif/", paste0(pft, collapse = "_"),"/", folder_name[index],  "/sPlot_TRYgapfilled_", name1, "_", folder_name[index], ".gri", sep="")
    print(filename1)
    writeRaster(r1, filename1, overwrite=TRUE, format="raster")
  }
  index <- index +1
}  

#test
# test <- brick("/net/home/tkattenborn/data_global_maps/trait_maps_gbif/Grass/2deg/GBIF_TRYgapfilled_X1080_2deg.gri")
# test
# plot(test)
