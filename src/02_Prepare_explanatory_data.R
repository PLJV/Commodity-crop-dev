#
#
# Author: Kyle Taylor (kyle.taylor@pljv.org) [2016]
#
# Cleaning-up and refactoring previous implementation so that it is pure 'R' and more closely follows functional
# design principles for v.2.0.  Adding the ability to generate training data across broader geographic extents than
# a single county, per v.1.0.

require(rgdal)
require(rgeos)
require(raster)
require(utils)
require(soilDB)
require(parallel)
require(FedData)
require(landscapeAnalysis)

argv <- commandArgs(trailingOnly=T)

#
# Local functions
#

# climate variables to be calculated from Reihfeldt
climate_variables <-
  c(
    "dd5.tif",
    "ffp.tif",
    "map.tif",
    "mat_tenths.tif"
  )

# topographic variables calculated from DEM
topographic_variables <-
  c(
    "elevation.tif",
    "elevationStdDev3.tif",
    "aspect.tif",
    "rough27.tif",
    "rough3.tif",
    "slope.tif"
  )

# SSURGO variables calculated from muaggatt tables
muaggatt_variables <-
 c(
   "aws025wta",       # Available Water Storage 0-25 cm
   "aws050wta",       # Available Water Storage 0-50 cm
   "aws0150wta",      # Available Water Storage 0-150 cm
   "brockdepmin",     # Bedrock Depth - Minimum
   "aws0100wta",      # Available Water Storage 0-100 cm
   "drclassdcd",      # Drainage Class - Dominant Condition
   "slopegraddcp",    # Slope Gradient - Dominant Component
   "slopegradwta",    # Slope Gradient - Weighted Average
   "wtdepannmin",     # Water Table Depth - Annual - Minimum
   "wtdepaprjunmin",  # Water Table Depth - April - June - Minimum
   "flodfreqdcd",     # Flooding Frequency - Dominant Condition
   "flodfreqmax",     # Flooding Frequency - Maximum
   "pondfreqprs",     # Ponding Frequency - Presence
   "hydgrpdcd",       # Hydrologic Group - Dominant Conditionsp
   "iccdcd",          # Irrigated Capability Class - Dominant Condition
   "iccdcdpct",       # Irrigated Capability Class  - Dominant Condition Aggregate Percent
   "hydclprs",        # Hydric Classification - Presence
   "niccdcd")         # Non-Irrigated Capability Class - Dominant Condition
#
# floodFrequencyToOrdinal()
# For floodfreqmax, floodfreqdcd
#
floodFrequencyToOrdinal <- function(x){
  x[grepl(x,pattern="None")] <- 1
  x[grepl(x,pattern="Very rare")] <- 2
  x[grepl(x,pattern="Rare")] <- 3
  x[grepl(x,pattern="Occasional")] <- 4
  x[grepl(x,pattern="Common")] <- 5
  x[grepl(x,pattern="Frequent")] <- 6
  x[grepl(x,pattern="Very frequent")] <- 7
  x[grepl(x,pattern="Not rated")] <- 2
  return(as.numeric(x)) # Anything looming here that isn't numeric will return NA
}
# For drclassdcd,drclasswettest
drainageToOrdinal <- function(x){
  x[grepl(x,pattern="Excessively drained")] <- 1
  x[grepl(x,pattern="Somewhat excessively drained")] <- 2
  x[grepl(x,pattern="Well drained")] <- 3
  x[grepl(x,pattern="Moderately well drained")] <- 4
  x[grepl(x,pattern="Somewhat poorly drained")] <- 5
  x[grepl(x,pattern="Poorly drained")] <- 6
  x[grepl(x,pattern="Very poorly drained")] <- 7
  x[grepl(x,pattern="Not rated")] <- NA
  return(as.numeric(x))
}
# For hydgrpdcd
hydgrpToOrdinal <- function(x){
  x[grepl(x,pattern="A")] <- 1
  x[grepl(x,pattern="B")] <- 2
  x[grepl(x,pattern="C")] <- 3
  x[grepl(x,pattern="D")] <- 4
  x[grepl(x,pattern="AD")] <- 5
  x[grepl(x,pattern="BD")] <- 6
  x[grepl(x,pattern="CD")] <- 7
  x[grepl(x,pattern="Not rated")] <- NA
  return(as.numeric(x))
}
# For hydclprs
hydclprsToOrdinal <- function(x){
  # note: these are now numeric 0-10, as of SSURGO v9
  x[grepl(x,pattern="All hydric")] <- 1
  x[grepl(x,pattern="Not hydric")] <- 2
  x[grepl(x,pattern="Partially hydric")] <- 3
  x[grepl(x,pattern="Unknown")] <- NA
  return(as.numeric(x))
}
#
# extentToSoilDBCoords()
#
extentToSoilDBCoords <- function(e,useFloorCeiling=F) {
  e<-as.vector(e)[c(1,3,2,4)]
  # NRCS gateway uses USGS web notation for its bounding box coords -- extend coverage to the
  # nearest degree to make sure we capture the full extent of polygons.  Can crop it down later, if needed.
  if(useFloorCeiling) e <- c(floor(e[1]),floor(e[2]),ceiling(e[3]),ceiling(e[4]))

  return(e)
}
#
# getMapunitGeomFile()
#
# Recursively dig through temp file space looking for a GML fetched by soilDB.  Check the
# file time stamp associated with all GML files and return the full path of the most recent
# file to the user.
#
getMapunitGeomFile <- function(){
  f <- list.files(path="/tmp",full.names=T,recursive=T,pattern="gml$")
  fTime <- unlist(strsplit(as.character(file.info(f)$mtime),split=' '))
    fTime <- fTime[grepl(fTime,pattern=':')]
  return(f[which(fTime == max(fTime))])
}
#
# calcMultiplier_mapunitGeomFile()
#
# Accepts a file path to GML markup and parses the file for keywords consistent with errors
# that we can use to optimize heuristics for further download attempts to find a good fit for the BBOX
# parameter for the soils gateway.
#
calcMultiplier_mapunitGeomFile <- function(x){
  o<-readLines(x)
  # if the thread was aborted, assume that it was because our BBOX was too large and we should step down
  if(grepl(tolower(o[length(o)-1]),pattern="thread was being aborted")){
    cat(" -- download aborted by gateway. check your bandwidth (are you downloading something else in the background?)\n")
    return(-0.005) # if we simply experienced an abort, assume that we are running on the heavy side, but not so heavy that it's outrageous.
  } else if(grepl(tolower(o[length(o)-1]),pattern="exceeds the limit of")){
    return(-0.005)
  } else if(grepl(tolower(o[length(o)-3]),pattern="null>missing<")){
    return(+0.005)
  } else {
    stop("unhandled error from mapunit_geom_by_ll_bbox; see:",x)
  }
}
#
# extentToSsurgoSpatialPolygons()
#
extentToSsurgoSpatialPolygons <- function(x){
  sizeHeurstics = data.frame(area=NA,multiplier=NA)
     multiplier = 1.015
  # NRCS gateway sees input in NAD83 by default -- also, it will not return an intersect.  Rather, it takes all intersecting features completely within a bounding box.
  # Buffer the current extent to allow for some wiggle-room.  We will walk down our extent as needed, through heuristic optimization.
  e <- extentToSoilDBCoords(multiplyExtent(extent(spTransform(x,CRS(projection("+init=epsg:4269")))),extentMultiplier=multiplier)) # NRCS gateway sees input in NAD83 by default
    sizeHeurstics[1,1] <- diff(c(e[1],e[3]))*diff(c(e[2],e[4])) # area of our bounding box in degrees^2
    sizeHeurstics[1,2] <- multiplier                            # multiplier constant applied for our initial step
  e <- try(mapunit_geom_by_ll_bbox(e))
  if(class(e) == "try-error"){
    while(class(e) == "try-error" && nrow(sizeHeurstics)<100){
      Sys.sleep(1) # hobble by 1 second to limit likelihood of race-conditions in file I/O
      cat("\n -- bounding-box heuristics optimizer, step:",nrow(sizeHeurstics),"\n\n")
      multiplier <- multiplier + calcMultiplier_mapunitGeomFile(getMapunitGeomFile())
      e <- extentToSoilDBCoords(multiplyExtent(extent(spTransform(x,CRS(projection("+init=epsg:4269")))),extentMultiplier=multiplier))
        sizeHeurstics <- rbind(sizeHeurstics,data.frame(area=diff(c(e[1],e[3]))*diff(c(e[2],e[4])),multiplier=multiplier))
      e <- try(mapunit_geom_by_ll_bbox(e));
    }
  }
  if(class(e) == "try-error") stop("repeated failure trying to download units in BBOX for focal area")

  projection(e) <- CRS(projection("+init=epsg:4269"))
    return(e)
}
#
# parseMuaggattTable()
#
parseMuaggattTable <- function(x,muaggatt.vars=NULL){
  in.statement <- format_SQL_in_statement(as.character(x@data$mukey))  # generate a SQL statement from MUKEYs
  q <- paste("SELECT muaggatt.mukey,", paste(muaggatt.vars,collapse=", "),
             " FROM muaggatt JOIN component ON muaggatt.mukey = component.mukey",
             " WHERE muaggatt.mukey IN ", in.statement,
             "ORDER BY muaggatt.mukey ASC", sep="")
  return(q)
}
#
# unpack()
#
unpackClimateVars <- function(x){
  filename <- substr(x,1,nchar(x)-4) # i.e., filename root; minus extension
  utils::unzip(x)
  if(file.rename(paste(filename,"txt",sep="."),paste(filename,"asc",sep="."))){
    r <- raster::raster(paste(filename,"asc",sep="."));
      raster::projection(r) <- raster::projection("+init=epsg:4326")
        return(r)
  } else {
    stop("couldn't read extracted climate .txt file from:",x)
  }
}
#
# fetchClimateVariables()
#
fetchClimateVariables <- function(){
  v <- c("dd5.zip","ffp.zip","map.zip","mat_tenths.zip")
  if(sum(grepl(list.files(pattern="zip$"),pattern=paste(v,collapse="|")))<length(v)){
    if(length(list.files(pattern="zip$"))==0){
      zips <- v
    } else {
      zips <- v[!grepl(v,pattern=paste(list.files(pattern="zip$"),collapse="|"))]
    }
    toFetch <- paste("http://forest.moscowfsl.wsu.edu/climate/current/allNA/derivedGrids/",zips,sep="")
      for(i in 1:length(toFetch)){ utils::download.file(toFetch[i], destfile=zips[i]) }
  }
  v <- c("dd5.zip","ffp.zip","map.zip","mat_tenths.zip")
  return(lapply(v,unpackClimateVars))
}
#
# fetchTopographicData()
# wrapper function for FedData::get_ned()
#
fetchTopographicData <- function(x,useLocal=FALSE){
  # parse list or individual raster object
  if(useLocal){
    return(lapply(topographic_variables,FUN=raster))
  }
  # calculate from a live DEM we fetch from NED
  if(!require(FedData)) stop("'fedData' package not available -- please install")
  x <- get_ned(template=x,res="1",label="dem",extraction.dir="/tmp",raw.dir="/tmp",force.redo=T)
  topo_output <- list()
    topo_output[[1]] <- x # elevation
    topo_output[[2]] <- raster::focal(x,w=matrix(1,nrow=3,ncol=3),fun=sd,na.rm=T) # StdDevElev (3x3)
    topo_output[[3]] <- raster::terrain(x,opt='aspect',neighbors=8)
    topo_output[[4]] <- raster::focal(x, w=matrix(1,nrow=27,ncol=27), fun=function(x, ...) max(x) - min(x),pad=TRUE, padValue=NA, na.rm=TRUE)
    topo_output[[5]] <- raster::focal(x, w=matrix(1,nrow=3,ncol=3), fun=function(x, ...) max(x) - min(x),pad=TRUE, padValue=NA, na.rm=TRUE)
    topo_output[[6]] <- raster::terrain(x,opt='slope',neighbors=8)
  return(topo_output)
}
#
# snapTo()
# Ensure absolute consistency between raster objects by cropping,projecting,snapping,and (if asked) resampling
# a raster object using a template
#
snapTo <- function(x,to=NULL,names=NULL,method='bilinear'){
  require(parallel)
  # set-up a cluster for parallelization
  cl <- makeCluster((parallel::detectCores()-1))
  # crop, reproject, and snap our raster to a resolution and projection consistent with the rest our explanatory data
  if(grepl(tolower(class(x)),pattern="character")){ lapply(x,FUN=raster) }
  e <- as(extent(to[[1]]),'SpatialPolygons')
    projection(e) <- CRS(projection(to[[1]]))
  if(class(x) == "list") {
    x <- parLapply(cl,x,fun=raster::crop,extent(spTransform(e,CRS(projection(x[[1]])))))
      x <- parLapply(cl,x,fun=raster::projectRaster,crs=CRS(projection(to[[1]])))
    extents <- lapply(x,alignExtent,to[[1]])
      for(i in 1:length(x)){ extent(x[[i]]) <- extents[[i]] }
    if(!is.null(method)){
      x <- parLapply(cl,x,fun=resample,y=to[[1]],method=method)
    }
  } else {
    x <- raster::crop(x,extent(spTransform(e,CRS(projection(x)))))
      x <- raster::projectRaster(x,crs=CRS(projection(to[[1]])))
    extent <- alignExtent(x,to[[1]])
      extent(x) <- extent
    if(!is.null(method)){
      x <- raster::resample(x,y=to[[1]],method=method)
    }
  }
  endCluster()
  return(x)
}
#
# lWriteRaster()
#
lWriteRaster <- function(x,y,cName=NULL){
  require(raster);
    raster::writeRaster(x,filename=paste(cName,"_",y,".tif",sep=""),overwrite=T,format="GTiff")
}

#
# MAIN
#

system("clear"); cat("## Commodity Crop Production Suitability Model (v.2.0) ##\n\n")

# accept input data from the user demonstrating the extent of our study area
s <- readOGR(".",argv[1],verbose=F)
# calculate our SSURGO soil variables, if needed
if(sum(grepl(list.files(pattern=paste(argv[1],".*.tif$",sep="")),pattern=paste(muaggatt_variables,collapse='|'))) < length(muaggatt_variables)){
  # set-up a cluster for parallelization
  cl <- makeCluster(parallel::detectCores()-1)
  # fetch and rasterize some SSURGO data
  county_polygons <- extentToSsurgoSpatialPolygons(s)
  # now get component and horizon-level data for these map unit keys
  res <- unique(SDA_query(parseMuaggattTable(county_polygons,muaggatt.vars=muaggatt_variables)))
  # reclassify some of our categorical information into ordinal classes

  # merge to our shapefile
  county_polygons@data <- merge(county_polygons@data,res,by="mukey")

  # convert our categorical variables to ordinal variables that are tractable for RandomForests
  county_polygons$flodfreqmax <- floodFrequencyToOrdinal(county_polygons$flodfreqmax)
  county_polygons$drclassdcd  <- drainageToOrdinal(county_polygons$drclassdcd)
  county_polygons$flodfreqdcd <- floodFrequencyToOrdinal(county_polygons$flodfreqdcd)
  county_polygons$hydgrpdcd   <- hydgrpToOrdinal(county_polygons$hydgrpdcd)

  # convert to rasters
  cat(" -- cropping SSURGO vectors to the extent of the focal county\n")
  county_polygons <- spTransform(county_polygons,CRS(projection(s)))
    county_polygons <- county_polygons[names(county_polygons)[!grepl(names(county_polygons),pattern="mu|vers|area")]] # drop unnecessary variables
        county_polygons <- raster::crop(county_polygons,s)
  cat(" -- generating gridded raster surfaces from SSURGO polygons\n")
  out <- list(); # today, my brain can't make splitting a SpatialPolygons file by field into a list happen for some reason
    for(i in 1:length(names(county_polygons))){ out[[length(out)+1]] <- county_polygons[,names(county_polygons)[i]] }
      f <- function(x,y=NULL,progress=NULL){ require(raster); return(rasterize(x[,1],raster(extent(x),res=30),update=T,field=names(x[,1]),progress=progress)) }
        out <- parLapply(cl,out,fun=f,progress=NULL)
          for(i in 1:length(out)){ lWriteRaster(out[[i]],y=names(county_polygons)[i],cName=argv[1]) }

  ssurgo_variables <- out
  # clean-up
  endCluster();
  rm(out,f);
} else {
  cat(paste(" -- existing SSURGO rasters found for ",argv[1],"; skipping generation and loading existing...\n",sep=""))
  out <- list.files(pattern=paste("^",argv[1],".*.tif$",sep=""))
    ssurgo_variables <- out[grepl(out,pattern=paste(muaggatt_variables,collapse="|"))]
      ssurgo_variables <- lapply(ssurgo_variables,FUN=raster)
}
# prepare our aquifer data
if(!file.exists(paste(argv[1],"_satThick_10_14.tif",sep=""))){
  cat(" -- cropping, resampling, and snapping aquifer saturated thickness so that it is consistent with our SSURGO variables\n")
  if(file.exists("satThick_10_14.tif")){
    aquiferSatThickness <- raster("satThick_10_14.tif")
      aquiferSatThickness <- projectRaster(aquiferSatThickness,crs=CRS(projection(s)))
        aquiferSatThickness <- crop(aquiferSatThickness,extent(s))
          aquiferSatThickness <- resample(aquiferSatThickness,ssurgo_variables[[1]],method='bilinear')
    extent(aquiferSatThickness) <- alignExtent(aquiferSatThickness,ssurgo_variables[[1]])
      writeRaster(aquiferSatThickness,paste(argv[1],"_satThick_10_14.tif",sep=""),overwrite=T)
  } else {
    stop("couldn't find an appropriate saturated thickness raster in the CWD")
  }
} else {
  cat(" -- existing saturated thickness raster found for",argv[1],"; skipping generation and loading existing...\n")
  aquiferSatThickness <- raster(paste(argv[1],"_satThick_10_14.tif",sep=""))
}
# prepare our climate data
if(sum(grepl(list.files(pattern=paste(argv[1],".*.tif$",sep="")),pattern=paste(climate_variables,collapse='|'))) < length(climate_variables)){
  # set-up a cluster for parallelization
  cl <- makeCluster((parallel::detectCores()-1))
  cat(" -- processing source climate data\n")
  names <- substr(climate_variables,1,nchar(climate_variables)-4)
  climate_variables <- fetchClimateVariables()
    # crop, reproject, and snap our raster to a resolution and projection consistent with the rest our explanatory data
    climate_variables <- parLapply(cl,climate_variables,fun=raster::crop,spTransform(s,CRS(projection(climate_variables[[1]]))))
      climate_variables <- parLapply(cl,climate_variables,fun=raster::projectRaster,crs=CRS(projection(s)))
        extents <- lapply(climate_variables,alignExtent,ssurgo_variables[[1]])
          for(i in 1:length(climate_variables)){ extent(climate_variables[[i]]) <- extents[[i]] }
    climate_variables <- parLapply(cl,climate_variables,fun=crop,landscapeAnalysis::multiplyExtent(extent(s))*1.05)
      climate_variables <- parLapply(cl,climate_variables,fun=resample,y=ssurgo_variables[[1]],method='bilinear')
        for(i in 1:length(climate_variables)){ lWriteRaster(climate_variables[[i]],y=names[i],cName=argv[1]) }
  endCluster()
} else {
  cat(paste(" -- existing climate rasters found for ",argv[1],"; skipping generation and loading existing...\n",sep=""))
  out <- list.files(pattern=paste("^",argv[1],".*.tif$",sep=""))
    climate_variables <- out[grepl(out,pattern=paste(climate_variables,collapse="|"))]
      climate_variables <- lapply(climate_variables,FUN=raster)
}

# calculate our topographic landscape variables
if(sum(grepl(list.files(pattern=paste(argv[1],".*.tif$",sep="")),pattern=paste(topographic_variables,collapse='|'))) < length(topographic_variables)){
  cat(" -- processing topographic data\n")
    names <- substr(topographic_variables,1,nchar(topographic_variables)-4)
    # fetch our topographic variables if they are not available locally
    topographic_variables <- fetchTopographicData(ssurgo_variables[[1]],useLocal=F)
    # snap to the extent and resolution of our ssurgo variables
    topographic_variables <- snapTo(topographic_variables,to=ssurgo_variables[[1]])
    # write to disk
    for(i in 1:length(topographic_variables)){ lWriteRaster(topographic_variables[[i]],y=names[i],cName=argv[1]) }
} else {
  cat(paste(" -- existing topographic rasters found for ",argv[1],"; skipping generation and loading existing...\n",sep=""))
  out <- list.files(pattern=paste("^",argv[1],".*.tif$",sep=""))
    topographic_variables <- out[grepl(out,pattern=paste(topographic_variables,collapse="|"))]
      topographic_variables <- lapply(topographic_variables,FUN=raster)
}
