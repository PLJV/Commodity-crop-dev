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
# reclassifyMuaggattCategoricals()
#
reclassifyMuaggattCategoricals <- function(x){
  if(length(musym <- ln[01]) == 0 || ln[01] == ""){ cat(" -- error: encountered an invalide musym value (this should never happen).\n"); stop(); }
  if(length(muname <- ln[02]) == 0 || ln[02] == ""){ cat(" -- error: encountered an invalide mukey value (this should never happen).\n"); stop(); }
  if(length(slopegraddcp <- ln[04]) == 0 || ln[04] == ""){ slopegraddcp <- NA }
  if(length(slopegradwta <- ln[05]) == 0 || ln[05] == ""){ slopegradwta <- NA }
  if(length(brockdepmin <- ln[06]) == 0 || ln[06] == ""){ brockdepmin <- NA }
  if(length(wtdepannmin  <- ln[07]) == 0 || ln[07] == ""){ wtdepannmin <- NA }
  if(length(wtdepaprjunmin <- ln[08]) == 0 || ln[08] == ""){ wtdepaprjunmin <- NA }
  if(length(flodfreqdcd <- ln[09]) == 0 || ln[09] == ""){
    flodfreqdcd  <- NA
  } else {
      if(flodfreqdcd == "None"){ flodfreqdcd <- 1
      } else if(flodfreqdcd == "Very rare"){ flodfreqdcd <- 2
      } else if(flodfreqdcd == "Rare"){ flodfreqdcd <- 3
      } else if(flodfreqdcd == "Occasional"){ flodfreqdcd <- 4
      } else if(flodfreqdcd == "Common"){ flodfreqdcd <- 5
      } else if(flodfreqdcd == "Frequent"){ flodfreqdcd <- 6
      } else if(flodfreqdcd == "Very frequent"){ flodfreqdcd <- 7
      } else { flodfreqdcd <- 99;
             cat(" -- encountered an unknown flood frequency class parsing muaggatt table.\n"); }
  }
  if(length(flodfreqmax<- ln[10]) == 0 || ln[10] == ""){ flodfreqdmax  <- NA
  } else {
       if (flodfreqmax == "None"){flodfreqmax <- 1
       } else if(flodfreqmax == 'Very rare'){ flodfreqmax <- 2
       } else if(flodfreqmax == 'Rare'){ flodfreqmax <- 3
       } else if(flodfreqmax == 'Occasional'){ flodfreqmax <- 4
       } else if(flodfreqmax == 'Common'){ flodfreqmax <- 5
       } else if(flodfreqmax == 'Frequent'){ flodfreqmax <- 6
       } else if(flodfreqmax == 'Very frequent'){ flodfreqmax <- 7
       } else if(flodfreqmax == 'Not rated'){ flodfreqmax <- NA
       } else{ flodfreqmax <- 99;
             cat(" -- encountered an unknown flood frequency max class parsing muaggatt table.\n"); }
  }
  if(length(pondfreqprs<- ln[11]) == 0 || ln[11] == "")  pondfreqprs <- NA
  if(length(aws0025wta <- ln[12]) == 0 || ln[12] == "")  aws0025wta  <- NA
  if(length(aws0050wta <- ln[13]) == 0 || ln[13] == "")  aws0050wta  <- NA
  if(length(aws0100wta <- ln[14]) == 0 || ln[14] == "")  aws0100wta  <- NA
  if(length(aws0150wta <- ln[15]) == 0 || ln[15] == "")  aws0150wta  <- NA
  if(length(drclassdcd <- ln[16]) == 0 || ln[16] == ""){ drclassdcd  <- NA
  } else {
      if(drclassdcd == "Excessively drained"){ drclassdcd <- 1
      } else if(drclassdcd == "Somewhat excessively drained"){ drclassdcd <- 2
      } else if(drclassdcd == "Well drained"){ drclassdcd <- 3
      } else if(drclassdcd == "Moderately well drained"){ drclassdcd <- 4
      } else if(drclassdcd == "Somewhat poorly drained"){ drclassdcd <- 5
      } else if(drclassdcd == "Poorly drained"){ drclassdcd <- 6
      } else if(drclassdcd == "Very poorly drained"){ drclassdcd <- 7
      } else if(drclassdcd == "Not rated"){ drclassdcd <- NA
      } else { drclassdcd <- 99;
             cat(" -- encountered an unknown dry drainage class parsing muaggatt table.\n"); }
  }
  if(length(drclasswettest <- ln[17]) == 0 || ln[17] == ""){ drclasswettest  <- NA
  } else {
      if(drclasswettest == "Excessively drained"){ drclasswettest <- 1
      } else if(drclasswettest == "Somewhat excessively drained"){ drclasswettest <- 2
      } else if(drclasswettest == "Well drained"){ drclasswettest <- 3
      } else if(drclasswettest == "Moderately well drained"){ drclasswettest <- 4
      } else if(drclasswettest == "Somewhat poorly drained"){ drclasswettest <- 5
      } else if(drclasswettest == "Poorly drained"){ drclasswettest <- 6
      } else if(drclasswettest == "Very poorly drained"){ drclasswettest <- 7
      } else if(drclasswettest == "Not rated"){ drclasswettest <- NA
      } else { drclasswettest <- 99;
             cat(" -- encountered an unknown wet drainage class (",drclasswettest,")parsing muaggatt table.\n", sep=""); }
  }
  if(length(hydgrpdcd <- ln[18]) == 0 || ln[18] == ""){ hydgrpdcd  <- NA
  } else {
      if(hydgrpdcd == "A" ){ hydgrpdcd <- 1
      } else if(hydgrpdcd == "B"){ hydgrpdcd <- 2
      } else if(hydgrpdcd == "C"){ hydgrpdcd <- 3
      } else if(hydgrpdcd == "D"){ hydgrpdcd <- 4
      } else if(hydgrpdcd == "AD"){ hydgrpdcd <- 5
      } else if(hydgrpdcd == "BD"){ hydgrpdcd <- 6
      } else if(hydgrpdcd == "CD"){ hydgrpdcd <- 7
      } else if(hydgrpdcd == "Not rated"){ hydgrpdcd <- 7
      } else if(suppressWarnings(!is.na(as.numeric(hydgrpdcd)))){ hydgrpdcd <- hydgrpdcd
      } else { cat(" -- encountered an unknown hydrologic group (",hydgrpdcd,") parsing muaggatt table.\n",sep="");
               hydgrpdcd <- 99;
      }
  }
  if(length(iccdcd <- ln[19]) == 0 || ln[19] == ""){ iccdcd  <- NA }
  if(length(iccdcdpct <- ln[20]) == 0 || ln[20] == "" ){ iccdcdpct  <- NA }
  if(length(niccdcd <- ln[21]) == 0 || ln[21] == "" ){ niccdcd  <- NA }
  if(length(niccdcdpct <- ln[22]) == 0 || ln[22] == "" ){ niccdcdpct  <- NA }
  if(length(hydclprs <- ln[38]) == 0 || ln[38] == "" ){ hydclprs  <- NA
  } else {
      # note: these are now numeric 0-10, as of SSURGO v9
      if(hydclprs == "All hydric"){ hydclprs <- 1
      } else if(hydclprs == "Not hydric"){ hydclprs <- 2
      } else if(hydclprs == "Partially hydric"){ hydclprs <- 3
      } else if(hydclprs == "Unknown"){ hydclprs <- 4
      } else if(suppressWarnings(!is.na(as.numeric(hydclprs)))) { hydclprs <- hydclprs
      } else {
        cat(" -- encountered an unknown hydrologic category (",hydclprs,") parsing muaggatt table.\n",sep="");
      }
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
  while(class(e)!="try-error" && errorCount<3){ try(mapunit_geom_by_ll_bbox(e)) }
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

pts <- readOGR(".",argv[1],verbose=F)

# fetch and rasterize some SSURGO data

 # q <- paste("SELECT component.cokey, mukey, compname,",paste(ssurgo_variables,collapse=", "),
 #            " FROM component JOIN chorizon ON component.cokey = chorizon.cokey",
 #            " WHERE majcompflag = 'Yes' AND mukey IN ", in.statement,
 #            "ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")


# now get component and horizon-level data for these map unit keys
res <- unique(SDA_query(parseMuaggattTable,muaggatt.vars=muaggatt_variables))
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
