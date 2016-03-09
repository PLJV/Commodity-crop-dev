#
#
# Author: Kyle Taylor (kyle.taylor@pljv.org) [2016]
#
# Cleaning-up and re-tooling previous implementation so that it is pure 'R' and more closely follows functional
# design principles for v.2.0.
#

require(rgdal)
require(raster)
require(utils)
require(soilDB)
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
  if (x == "None"){ x <- 1
  } else if(x == 'Very rare'){ x <- 2
  } else if(x == 'Rare'){ x <- 3
  } else if(x == 'Occasional'){ x <- 4
  } else if(x == 'Common'){ x <- 5
  } else if(x == 'Frequent'){ x <- 6
  } else if(x == 'Very frequent'){ x <- 7
  } else if(x == 'Not rated'){ x <- NA
  } else{ x <- NA;
        cat(" -- encountered an unknown flood frequency class parsing muaggatt table.\n");
  }
}
# For drclassdcd,drclasswettest
floodFrequencyToOrdinal <- function(x){
  if(x == "Excessively drained"){ drclassdcd <- 1
  } else if(x == "Somewhat excessively drained"){ x <- 2
  } else if(x == "Well drained"){ x <- 3
  } else if(x == "Moderately well drained"){ x <- 4
  } else if(x == "Somewhat poorly drained"){ x <- 5
  } else if(x == "Poorly drained"){ x <- 6
  } else if(x == "Very poorly drained"){ x <- 7
  } else if(x == "Not rated"){ x <- NA
  } else { x <- NA;
         cat(" -- encountered an unknown drainage class parsing muaggatt table.\n"); }
}
# For hydgrpdcd
hydgrpToOrdinal <- function(x){
  if(x == "A" ){ x <- 1
  } else if(x == "B"){ x <- 2
  } else if(x == "C"){ x <- 3
  } else if(x == "D"){ x <- 4
  } else if(x == "AD"){ x <- 5
  } else if(x == "BD"){ x <- 6
  } else if(x == "CD"){ x <- 7
  } else if(x == "Not rated"){ x <- 7
  } else if(suppressWarnings(!is.na(as.numeric(x)))){ x <- x
  } else { cat(" -- encountered an unknown hydrologic group (",x,") parsing muaggatt table.\n",sep="");
           hydgrpdcd <- NA;
  }
}
# For hydclprs
hydclprsToOrdinal <- function(x){
  # note: these are now numeric 0-10, as of SSURGO v9
  if(x == "All hydric"){ x <- 1
  } else if(x == "Not hydric"){ x <- 2
  } else if(x == "Partially hydric"){ x <- 3
  } else if(x == "Unknown"){ x <- 4
  } else if(suppressWarnings(!is.na(as.numeric(x)))) { x <- x
  } else {
    cat(" -- encountered an unknown hydrologic category (",x,") parsing muaggatt table.\n",sep="");
  }
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
#
#
get_mapunitGeomFile <- function(){
  f <- list.files(path="/tmp",full.names=T,recursive=T,pattern="gml$")

  fTime <- unlist(strsplit(as.character(file.info(f)$mtime),split=' '))
    fTime <- fTime[grepl(fTime,pattern=':')]

  return(f[which(fTime == max(fTime))])
}
#
#
#
calcMultiplier_mapunitGeomFile <- function(x){
  o<-readLines(x)
  # if the thread was aborted, assume that it was because our BBOX was too large and we should step down
  if(grepl(tolower(o[length(o)-1]),pattern="thread was being aborted")){
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
    sizeHeurstics[1,1] <- diff(c(e[1],e[3]))*diff(c(e[2],e[4]))
    sizeHeurstics[1,2] <- multiplier
  e <- try(mapunit_geom_by_ll_bbox(e))
  if(class(e) == "try-error"){
    while(class(e) == "try-error" && nrow(sizeHeurstics)<100){
      Sys.sleep(1) # hobble by 1 second to prevent race-condition in file I/O
      cat(" -- heuristics step:",nrow(sizeHeurstics),"\n")
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
# MAIN
#

# accept input data from the user demonstrating the extent of our study area
s <- readOGR(".",argv[1],verbose=F)
# fetch and rasterize some SSURGO data
e <- extentToSsurgoSpatialPolygons(s)
# now get component and horizon-level data for these map unit keys
res <- unique(SDA_query(parseMuaggattTable(e,muaggatt.vars=muaggatt_variables)))
# reclassify some of our categorical information into ordinal classes

# merge to our shapefile
e@data <- merge(e@data,res,by="mukey")
# convert to a raster
cat(" -- generating gridded raster surfaces from SSURGO polygons")
projection(e) <- projection("+init=epsg:4269")
  e <- spTransform(e,CRS(projection(pts)))
    e <- rasterize(e,raster(e,res=30),field=names(e)[!grepl(names(e),pattern="mu|vers|area")],progress='text')
muaggatt_variables <- e; rm(e)

# prepare our aquifer data
# prepare our climate data
# calculate our topographic landscape variables
