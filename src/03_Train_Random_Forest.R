require(raster)
require(rgdal)
require(randomForest)
require(rfUtilities)

argv <- commandArgs(trailingOnly=T)

#
# Local functions
#

#
# qaCheck_dropVarsWithAbundantNAs()
#
qaCheck_dropVarsWithAbundantNAs <- function(t){
  names <- names(t)
  for(n in names){
    if(sum(is.na(t[,n]))/nrow(t) > 0.8){
      cat(" -- (warning) dropping variable ",n," from consideration, because it would result in a ",(sum(is.na(t[,n]))/nrow(t)*100), "% loss of training data due to NA values\n",sep="")
      t <- t[,names(t)!=n]
    }
  }
  return(t)
}
#
# qaCheck_checkBalancedClasses()
#
qaCheck_checkBalancedClasses<- function(t,correct=F){
  t <- na.omit(t) # we will have to omit some records before training -- what will that do to our class balance?
  ratio <- sum(t$response==0)/nrow(t)
  if(ratio > 0.65 || ratio < 0.45) {
    cat(" -- (warning) there's a fairly large class imbalance observed in the data. Reponse 0/1 : ",sum(t$response==0)/nrow(t),"\n",sep="")
  }
  if(correct){
    cat(" -- corrected by downsampling to less abundant class\n")
  }
  if(nrow(t)==0){
    stop("Training table is completely NA.  This shouldn't happen.")
  }
  return(t)
}
#
# qaCheck_multiColinearity()
#
qaCheck_multiColinearity <- function(t){
  coVars <- suppressWarnings(rfUtilities::multi.collinear(training_table))
  if(length(coVars)>0){
    cat(" -- (warning) dropped variables due to multicolinearity:",coVars,"\n",sep="")
    t <- t[,grepl(names(t),pattern=paste(coVars,collapse="|"))]
  }
  return(t)
}
#
#
#
qaCheck_dropVarsWithPoorExplanatoryPower <- function(m=NULL,t=NULL,p=0.35,plot=FALSE){
  o <- rfUtilities::rf.imp.freq(m,p=p,plot=plot)
    o$frequency$var.freq <- o$frequency$var.freq/3
  if(sum(o$frequency$var.freq == 0)>0){
    cat(" -- (warning) dropping uninformative variables :",as.vector(o$frequency$vars)[o$frequency$var.freq == 0],"\n",sep="")
    t <- t[,grepl(names(t),pattern=paste(as.vector(o$frequency$vars)[o$frequency$var.freq > 0.3],collapse="|"))]
  }
  return(t)
}
#
# MAIN
#

# read-in our training data
training_pts <- readOGR(".",paste(argv,"_farmed_binary_pts",sep=""),verbose=F)
# read-in our explanatory data
expl_vars <- list.files(pattern=paste(argv[1],"*.*tif$",sep=""))
  expl_vars <- expl_vars[!grepl(expl_vars,pattern="farmed")]
    expl_vars <- lapply(expl_vars,FUN=raster)
      expl_vars <- try(raster::stack(expl_vars))
names <- names(expl_vars)
  names <- names[!grepl(names,pattern="county|focal")]
    names <- names[suppressWarnings(is.na(as.numeric(names)))]
      names <- names[which(grepl(names(expl_vars),pattern=paste(names,collapse="|")))]
        names(expl_vars) <- names
# extract across our training points
training_pts <- training_pts[!is.na(extract(expl_vars$iccdcdpct,training_pts)),] # the geometry of our SSURGO data can be limiting here...
training_table <- extract(expl_vars,training_pts,df=T)
  training_table <- cbind(data.frame(response=training_pts$response),training_table[,!grepl(names(training_table),pattern="ID$")])

# QA Check our training data
training_table$slope[is.na(training_table$slope)] <- 0

training_table <- qaCheck_dropVarsWithAbundantNAs(training_table)
training_table <- qaCheck_checkBalancedClasses(training_table)
training_table <- qaCheck_multiColinearity(training_table)

# train a preliminary forest
m <- randomForest(as.factor(response)~.,data=training_table,importance=T,ntree=3000,do.trace=T)
# Post-hoc QA check variable importance
training_table <- qaCheck_dropVarsWithPoorExplanatoryPower(m,t=training_table)

#
