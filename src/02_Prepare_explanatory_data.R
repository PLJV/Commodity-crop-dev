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
extentToSoilDBCoords <- function(e) round(as.vector(e)[c(1,3,2,4)],2) # NRCS gateway uses USGS web notation for its bounding box coords, accurate to 2 decimal places
#
# extentToSsurgoSpatialPolygons()
#
extentToSsurgoSpatialPolygons <- function(x){
  e <- extentToSoilDBCoords(extent(spTransform(x,CRS(projection("+init=epsg:4269"))))) # NRCS gateway sees input in NAD83 by default
   e <- try(mapunit_geom_by_ll_bbox(e))
  errorCount <- 0;
  while(class(e) == "try-error" && errorCount<3){
    Sys.sleep(2); # hobble our request for the sake of the NRCS gateway
    e <- try(mapunit_geom_by_ll_bbox(extentToSoilDBCoords(extent(spTransform(x,CRS(projection("+init=epsg:4269")))))));
    errorCount<-errorCount+1;
  }
  if(class(e) == "try-error") quit("repeated failure trying to download units in BBOX for county")
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
pts <- readOGR(".",argv[1],verbose=F)
# fetch and rasterize some SSURGO data
e <- extentToSsurgoSpatialPolygons(pts)
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
