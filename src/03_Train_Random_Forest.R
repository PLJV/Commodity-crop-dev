require(raster)
require(rgdal)
require(randomForest)
require(rfUtilities)

#argv <- commandArgs(trailingOnly=T)
argv <- "focal_county_146"

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
  coVars <- suppressWarnings(rfUtilities::multi.collinear(t))
  if(length(coVars)>0){
    cat(" -- (warning) dropped variables due to multicolinearity:",coVars,"\n",sep="")
    t <- t[,!grepl(names(t),pattern=paste(coVars,collapse="|"))]
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
    cat(" -- (warning) dropping uninformative variables :",paste(as.vector(o$frequency$vars)[o$frequency$var.freq == 0],collapse=" "),"\n",sep="")
    t <- cbind(response=t[,'response'],
               t[,grepl(names(t),pattern=paste(as.vector(o$frequency$vars)[o$frequency$var.freq > 0.3],collapse="|"))])
  }
  return(t)
}
#
# chunk()
#
chunk <- function(x,size=200){
  split(x, ceiling(seq_along(x)/size))
}
#
# qaCheck_findConvergence()
# Use a quick likelihood ratio to find chunks that are significantly (p>=95%) different than the tail of
# OOB error observed in a random forest object.  Report the right-hand side (upper-bound) of the last significant
# chunk back to the user to help establish a cut-off for re-training a forest.  This is an approximation of a moving
# window analysis (or perhaps, a non-moving window analysis... ;-))to find a value appropriate for twice-the-rate-of-convergence
# rule usually applied to picking an appropriate ntrees parameter for randomForest.
#
qaCheck_findConvergence <- function(m=NULL,chunkSize=100){
  err <- abs(diff(diff(m$err.rate[,1])))
    err <- chunk(err,size=chunkSize)

  tailTrainingData <- as.vector(unlist(err[length(err)-3:length(err)]))
    tailTrainingData <- c(mean(tailTrainingData),sd(tailTrainingData))

  means <- as.vector(unlist(lapply(err,FUN=mean)))
    sds <- as.vector(unlist(lapply(err,FUN=sd)))

  sigs <- dnorm(x=tailTrainingData[1],mean=tailTrainingData[1],sd=tailTrainingData[2],log=F)/dnorm(x=means[1:length(err)],mean=tailTrainingData[1],sd=tailTrainingData[2],log=F)
    sigs <- sigs >= 1.96
      sigs <- max(which(sigs==TRUE))

  return(sigs*chunkSize)
}
#
# MAIN
#
cat(" -- training random forests\n")
# read-in our training data
training_pts <- readOGR(".",paste(argv,"_farmed_binary_pts",sep=""),verbose=F)
# read-in our explanatory data
expl_vars <- list.files(pattern=paste("^",argv[1],".*.tif$",sep=""))
  expl_vars <- expl_vars[!grepl(expl_vars,pattern="farmed")]
    expl_vars <- lapply(expl_vars,FUN=raster)
      expl_vars <- raster::stack(expl_vars)
names <- names(expl_vars)
  names <- unlist(strsplit(names,split="_"))
    names <- names[!grepl(names,pattern="county|focal")]
      names <- names[suppressWarnings(is.na(as.numeric(names)))]
        names <- names[which(grepl(names(expl_vars),pattern=paste(names,collapse="|")))]
          names(expl_vars) <- names
# extract across our training points
training_pts_ <- training_pts[!is.na(extract(subset(expl_vars,subset='iccdcdpct'),training_pts)),] # the geometry of our SSURGO data can be limiting here...
  if(class(training_pts_) == "try-error") { rm(training_pts_) } else { training_pts <- training_pts_; rm(training_pts_) }
    training_table <- extract(expl_vars,training_pts,df=T)
      training_table <- cbind(data.frame(response=training_pts$response),training_table[,!grepl(names(training_table),pattern="ID$")])
# QA Check our training data
training_table$slope[is.na(training_table$slope)] <- 0

training_table <- qaCheck_dropVarsWithAbundantNAs(training_table)
training_table <- qaCheck_checkBalancedClasses(training_table)
training_table <- qaCheck_multiColinearity(training_table)
# Train a preliminary forest

cat("## Preliminary Burn-in/Evaluative Forest ##")
m <- randomForest(as.factor(response)~.,data=training_table,importance=T,ntree=3000,do.trace=T)
# Post-hoc QA check variable importance
training_table <- qaCheck_dropVarsWithPoorExplanatoryPower(m,t=training_table)

cat(" -- re-training a final forest, optimizing based on 2X convergence of OOB error in the final model")
m <- randomForest(as.factor(response)~.,data=training_table,importance=T,ntree=3000,do.trace=T)
  rf.final <- randomForest(as.factor(response)~.,data=training_table,importance=T,ntree=qaCheck_findConvergence(m,chunkSize=10)*2,do.trace=F)

cat(" -- predicting across explanatory raster series for focal county:\n")
  r_projected <- subset(expl_vars,subset=which(grepl(names(expl_vars),pattern=paste(names(training_table)[names(training_table)!="response"],collapse="|")))) # subset our original raster stack to only include our "keeper" variables
    r_predicted <- predict(r_projected,model=rf.final,progress='text',type='prob',na.rm=T,inf.rm=T,index=2) # index=2 specifies the "occurrence" (1) class from our forest

cat(" -- saving results to disk\n")
session <- new.env()
assign("rf.final",value=m,env=session)
  assign("training_table",value=training_table,env=session)
    assign("expl_vars",value=expl_vars,env=session)
  assign("training_pts",value=training_pts,env=session)
assign("r_predicted",value=r_predicted,env=session)

writeRaster(r_predicted,paste(argv[1],"_prob_occ.tif",sep=""),overwrite=T)
save(list=ls(session),envir=session,file=paste(argv[1],"_model.rdata",sep=""),compress=T)

cat(" -- done\n")
