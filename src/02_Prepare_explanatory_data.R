require(rgdal)
require(raster)
require(utils)
require(soilDB)
require(landscapeAnalysis)

argv <- commandArgs(trailingOnly=T)

extentToSoilDBCoords <- function(e) round(as.vector(e)[c(1,3,2,4)],2) # NRCS gateway uses USGS web notation for its bounding box coords, accurate to 2 decimal places

#
# MAIN
#

pts <- readOGR(".",argv[1],verbose=F)

# fetch and rasterize some SSURGO data
e <- extentToSoilDBCoords(extent(spTransform(pts,CRS(projection("+init=epsg:4326"))))) # NRCS gateway sees in WGS84
 e <- mapunit_geom_by_ll_bbox(e)

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

            m <- as.character(e@data$mukey)              # confirm our requisite MUKEYs for each vector
 in.statement <- format_SQL_in_statement(m)              # generate a SQL statement from MUKEYs

 # q <- paste("SELECT component.cokey, mukey, compname,",paste(ssurgo_variables,collapse=", "),
 #            " FROM component JOIN chorizon ON component.cokey = chorizon.cokey",
 #            " WHERE majcompflag = 'Yes' AND mukey IN ", in.statement,
 #            "ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")

 q <- paste("SELECT muaggatt.mukey,", paste(muaggatt_variables,collapse=", "),
            " FROM muaggatt JOIN component ON muaggatt.mukey = component.mukey",
            " WHERE muaggatt.mukey IN ", in.statement,
            "ORDER BY muaggatt.mukey ASC", sep="")
# now get component and horizon-level data for these map unit keys
res <- SDA_query(q)
# merge to our shapefile
x@data <- merge(e@data,res[,1:ncol(res)],by="mukey")
# plot with some pretty colors
plot(e,col=e$claytotal_r)
# prepare our aquifer data
# prepare our climate data
# calculate our topographic landscape variables
