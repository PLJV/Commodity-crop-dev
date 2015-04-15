#######################################################################################
#######################################################################################
# THIS CREATES A NEW PROJECT WORKSPACE WITH A DIRECTORY STRUCTURE COMMENSERATE TO WHAT 
# THE TILLAGE LIKELIHOOD MODEL IS USED TO WORKING WITH.  
#
# ACCEPTS 3 ARGUMENTS, [1] THE PATH TO AN ESRI SHAPEFILE (.SHP) CONTAINING ALL OF THE 
# COUNTIES IN WHICH YOU WANT TO RUN THE MODEL, WITH AN FID FIELD AND COUNTRY NAME FIELD 
# IN ITS ATTRIBUTE TABLE, [2] THE PATH TO A State-level SSURGO zip file (as downloaded from NRCS),
# AND [3] THE DESTINATION (ROOT) FOLDER IN WHICH TO CREATE THE MODEL WORKSPACE DIRECTORIES.
#
# NOTE: FIPS AND SSURGO DO NOT ALWAYS AGREE ON THE BOUNDARIES OF A COUNTY.  MAKE SURE YOUR
# SHAPEFILE IS CONSISTENT WITH THE SSURGO FIPS BOUNDARIES.
#
#######################################################################################
#######################################################################################

require(rgdal, quietly=T)
require(raster, quietly=T)
require(gdata, quietly=T)

# local includes
source("src/ktaylor_essentialSpatialAddons.R")
source("src/createSSURGORasters.R")

argv <- commandArgs(trailingOnly=T)

if(length(argv) != 3){
  cat(" -- error: BuildProjectWorkspace accepts 3 arguments.\n")
  stop()
}

# workspace

tillageModelRoot <- getwd()

regionalElevation <- raster("data/elevation.tif")
regionalClimate   <- paste("data",c("adi.tif","dd5.tif","ffp.tif","map.tif","mat.tif"), sep="/")
  regionalClimate <- lapply(as.list(regionalClimate), FUN=raster)

   s <- strsplit(argv[1], split="/")[[1]] # SOURCE SHAPEFILE, CONTAINING COUNTIES WITH A FIPS DESIGNATION

dsn   <- paste(s[1:length(s)-1], collapse="/")
layer <- strsplit(s[length(s)], split="[.]")[[1]][1]

#
# sanity-check our input data
#

cat(" -- sanity checking input data.\n")

if(file.exists(argv[2]) && tools::file_ext(argv[2]) == "zip"){ 
  ssurgoStateZip <- argv[2] 
}

projectWorkspace <- argv[3]

if(file.exists(argv[3])){ # DESTINATION FOLDER FOR OUR PROJECT / WORKSPACE
	setwd(argv[3])
} else {
	dir.create(argv[3])
	setwd(argv[3])
}

inshapeCounties <- try(readOGR(dsn, layer, verbose=F))

if(class(inshapeCounties) == "try-error") { 
  cat(" -- error: couldn't read shapefile,", layer, "\n", sep=" "); 
  stop(); 
} else if(!sum(c("FIPS","NAME") %in% colnames(inshapeCounties@data))){ 
  cat(" -- error: One or both of the FIPS and (county) NAME fields are missing from attribute table in", layer, ", please ammend.\n", sep=" "); 
  stop(); 
}


#
# build our directory tree from FIPS designations
#

cat(" -- building template directory structures.\n")
counties <- as.vector(inshapeCounties@data$FIPS)
for(d in counties){ 
  focal <- paste(getwd(), d, sep="/"); 
    focal <- c(focal, paste(focal,"RASTER",sep="/"), paste(focal,"VECTOR",sep="/")); 
  for(f in focal) { dir.create(f,recursive=F); }
}

#
# crop elevation and weather data to focal county FIPS
#

cat(" -- cropping elevation and climate data to focal counties : \n");
cat("  -- elevation : ")
 elev <- cropRasterByPolygons(r=regionalElevation,  s=inshapeCounties, field="FIPS", write=F); 
cat("  -- adi : ")
 adi <- cropRasterByPolygons(r=regionalClimate[[1]], s=inshapeCounties, field="FIPS", write=F); 
cat("  -- dd5 : ")
 dd5 <- cropRasterByPolygons(r=regionalClimate[[2]], s=inshapeCounties, field="FIPS", write=F); 
cat("  -- ffp : ")
 ffp <- cropRasterByPolygons(r=regionalClimate[[3]], s=inshapeCounties, field="FIPS", write=F); 
cat("  -- map : ")
 map <- cropRasterByPolygons(r=regionalClimate[[4]], s=inshapeCounties, field="FIPS", write=F); 
cat("  -- mat : ")
 mat <- cropRasterByPolygons(r=regionalClimate[[5]], s=inshapeCounties, field="FIPS", write=F); 
 
for(d in counties){
  focal <- paste(getwd(), d, "RASTER", "elev.tif", sep="/")
    writeRaster(elev[[which(counties %in% d)]], filename=focal)
  focal <- paste(getwd(), d, "RASTER", "map.tif", sep="/")
    writeRaster(map[[which(counties %in% d)]], filename=focal)
  focal <- paste(getwd(), d, "RASTER", "mat.tif", sep="/")
    writeRaster(mat[[which(counties %in% d)]], filename=focal)
  focal <- paste(getwd(), d, "RASTER", "adi.tif", sep="/")
    writeRaster(adi[[which(counties %in% d)]], filename=focal)
  focal <- paste(getwd(), d, "RASTER", "ffp.tif", sep="/")
    writeRaster(ffp[[which(counties %in% d)]], filename=focal)
  focal <- paste(getwd(), d, "RASTER", "dd5.tif", sep="/")
    writeRaster(dd5[[which(counties %in% d)]], filename=focal)
}

setwd("..")

#
# create SSURGO rasters for focal counties, honoring our project directory structure
#
cat(" -- unpacking state SSURGO data\n")
countyZipFiles <- ssurgoUnpackState(ssurgoStateZip, exdir=projectWorkspace)
cat(" -- rasterizing SSURGO data for focal counties : \n");
state <- tolower(substr(counties[1],start=1,stop=2));
# Read-in the NCCPI productivity table for the current state
nccpi_t <- populateNccpiTable(path=paste(tillageModelRoot, "/data/", 
                                      state,"_nccpi.xls", sep=""))

setwd(projectWorkspace)

for(d in counties){ 
  cat(" -- unpacking and processing ", d, " : \n", sep="");
  # unpack our SSURGO data into the current FIPS directory
  setwd(d)
  ssurgoUnpackCounty(path=paste("..",countyZipFiles[which(grepl(countyZipFiles, pattern=d))],sep="/")) # unpack to a replicate subdirectory, (e.g., TX123/TX123)
  dsn=paste(d,"spatial",sep="/")
    l=list.files(dsn)[1]
      l=substr(l,start=1, stop=nchar(l)-4)
        s<-readOGR(dsn=dsn, layer=l,verbose=F)
  # parse the muaggatt table and rasterize the SSURGO polygons
  muaggatt_t <- populateMuaggattTable(path=d) # parse the muaggatt for the focal FIPS
           r <- ssurgoVectorToRaster(s=s,muaggattTable=muaggatt_t,nccpiTable=nccpi_t)
  # write SSURGO rasters to disk
  setwd("RASTER")
  for(j in 1:length(r)){
    writeRaster(r[[j]], filename=paste(r[[j]]@data@names,"tif",sep="."))
  }
  # write a copy of our polygons shapefile to disk.  They'll be used to generate a point sample later (02)
  setwd("../VECTOR")
  t<-try(writeOGR(s,".","SSURGO", driver="ESRI Shapefile")) 
  if(class(t) == "try-error") stop("failed to write SSURGO shapefile to VECTOR directory")
  # clean-up
  setwd("..")
    unlink(d)
      setwd("..")
}

cat(" -- done.\n")
