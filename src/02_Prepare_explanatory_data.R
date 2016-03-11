#
#
# Author: Kyle Taylor (kyle.taylor@pljv.org) [2016]
#
# Cleaning-up and refactoring previous implementation so that it is pure 'R' and more closely follows functional
# design principles for v.2.0.  Adding the ability to generate training data across broader geographic extents than
# a single county, per v.1.0.

require(rgdal)
require(raster)
require(utils)
require(soilDB)
require(parallel)
require(landscapeAnalysis)

argv <- commandArgs(trailingOnly=T)

#
# Local functions
#

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
# get_mapunitGeomFile()
#
# Recursively dig through temp file space looking for a GML fetched by soilDB.  Check the
# file time stamp associated with all GML files and return the full path of the most recent
# file to the user.
#
get_mapunitGeomFile <- function(){
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
      cat("\n -- heuristics step:",nrow(sizeHeurstics),"\n")
      multiplier <- multiplier + calcMultiplier_mapunitGeomFile(get_mapunitGeomFile())
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
# fetchClimateVariables()
#
fetchClimateVariables <- function(){
  v <- c("dd5.zip","ffp.zip","map.zip","mat_tenths.zip")
  if(sum(grepl(list.files(pattern="zip$"),pattern=paste(v,collapse="|")))<length(v)){
    toFetch <- v[!grepl(list.files(pattern="zip$"),pattern=paste(v,collapse="|"))]
      toFetch <- paste("http://forest.moscowfsl.wsu.edu/climate/current/allNA/derivedGrids/",toFetch,sep="")
    lapply(toFetch,FUN=utils::download.file)
  }
  
}

#
# MAIN
#

cl <- makeCluster(parallel::detectCores()-1)

# accept input data from the user demonstrating the extent of our study area
s <- readOGR(".",argv[1],verbose=F)
# calculate our SSURGO soil variables, if needed
if(sum(grepl(list.files(pattern=paste(argv[1],".*.tif$",sep="")),pattern=paste(muaggatt_variables,collapse='|'))) < length(muaggatt_variables)){
  # fetch and rasterize some SSURGO data
  o <- extentToSsurgoSpatialPolygons(s)
  # now get component and horizon-level data for these map unit keys
  res <- unique(SDA_query(parseMuaggattTable(o,muaggatt.vars=muaggatt_variables)))
  # reclassify some of our categorical information into ordinal classes

  # merge to our shapefile
  o@data <- merge(o@data,res,by="mukey")

  # convert our categorical variables to ordinal variables that are tractable for RandomForests
  o$flodfreqmax <- floodFrequencyToOrdinal(o$flodfreqmax)
  o$drclassdcd  <- drainageToOrdinal(o$drclassdcd)
  o$flodfreqdcd <- floodFrequencyToOrdinal(o$flodfreqdcd)
  o$hydgrpdcd   <- hydgrpToOrdinal(o$hydgrpdcd)

  # convert to rasters
  cat(" -- cropping SSURGO vectors to the extent of the focal county\n")
  o <- spTransform(o,CRS(projection(s)))
    o <- o[names(o)[!grepl(names(o),pattern="mu|vers|area")]] # drop unnecessary variables
        o <- raster::crop(o,s)
  cat(" -- generating gridded raster surfaces from SSURGO polygons\n")
  out <- list(); # today, my brain can't make splitting a SpatialPolygons file by field into a list happen for some reason
    for(i in 1:length(names(o))){ out[[length(out)+1]] <- o[,names(o)[i]] }
      f <- function(x,y=NULL,progress=NULL){ require(raster); return(rasterize(x[,1],raster(extent(x),res=30),update=T,field=names(x[,1]),progress=progress)) }
        out <- parLapply(cl,out,fun=f,progress=NULL)
          f <- function(x,y,cName=NULL){ require(raster); writeRaster(x,filename=paste(cName,"_",y,".tif",sep="",overwrite=T),format="GTiff") }
            for(i in 1:length(out)){ f(out[[i]],y=names(o)[i],cName=argv[1]) }
  # clean-up
  endCluster();
  rm(o,f,cl);
} else {
  cat(paste(" -- existing SSURGO rasters found for ",argv[1],"; skipping generation and loading existing...\n",sep=""))
  out <- list.files(pattern=paste("^",argv[1],".*.tif$",sep=""))
    out <- out[grepl(out,pattern=paste(muaggatt_variables,collapse="|"))]
      out <- lapply(out,FUN=raster)
}
# prepare our aquifer data
if(!file.exists(paste(argv[1],"_satThick_10_14.tif",sep=""))){
  cat(" -- cropping, resampling, and snapping aquifer saturated thickness so that it is consistent with our SSURGO variables\n")
  if(file.exists("satThick_10_14.tif")){
    aquiferSatThickness <- raster("satThick_10_14.tif")
      aquiferSatThickness <- projectRaster(aquiferSatThickness,crs=CRS(projection(s)))
        aquiferSatThickness <- crop(aquiferSatThickness,landscapeAnalysis::multiplyExtent(extent(s))*1.05)
          aquiferSatThickness <- resample(aquiferSatThickness,out[[1]],method='bilinear')
    extent(aquiferSatThickness) <- alignExtent(aquiferSatThickness,out[[1]])
      writeRaster(aquiferSatThickness,paste(argv[1],"_satThick_10_14.tif",sep=""),overwrite=T)
  } else {
    stop("couldn't find an appropriate saturated thickness raster in the CWD")
  }
}
# prepare our climate data

# calculate our topographic landscape variables
