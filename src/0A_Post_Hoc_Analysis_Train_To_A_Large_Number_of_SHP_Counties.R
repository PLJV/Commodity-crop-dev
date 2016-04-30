source("02_Prepare_explanatory_data.R")

s<-readOGR("source_counties","aggregate_southern_hp_counties")

o<-splitExtent(e=extent(s),multiple=6)
  o<-lapply(o,as,'SpatialPolygons')
    for(i in 1:length(o)) { projection(o[[i]]) <- projection(s) }

o <- lapply(o,FUN=extentToSsurgoSpatialPolygons)
  o <- lapply(o,FUN=mosaic)
