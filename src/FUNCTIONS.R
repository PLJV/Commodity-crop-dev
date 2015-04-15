require(raster)
require(sp)
require(rgdal)
require(compiler)
require(randomForest)
require(MASS)

###################################################################
# PROGRAM: MultiColinear.R                
# PURPOSE: IDENTIFY MULTI-COLINEAR VARIABLES USING QR MATRIX DECOMPOSITION                   
#
# ARGUMENTS: 
#       X   A DATAFRAME 
#       p   MULTI-COLINEARITY THRESHOLD (DEFAULT 1e-07)
#
# VALUE:
#       TEST STATISTIC MESSAGE
#       CHARACTER VECTOR OF POTENTIAL MULTI-COLINEAR VARIABLES
#
# NOTES:
#       COLINEARITY THRESHOLDS MUST BE ADJUSTED BASED ON NUMBER OF 
#        X-VARIABLES. FOR SMALL NUMBER OF VARIABLES (<20) USE 1e-07 
#        FOR LARGE NUMBER (>20) USE 0.05 
#
# EXAMPLES: 
#   # DUMMY DATA
#   test = data.frame(v1=seq(0.1, 5, length=100), v2=seq(0.1, 5, length=100), 
#                     v3=dnorm(runif(100)), v4=dnorm(runif(100)) ) 
#
#   # TEST FOR MULTICOLINEAR VARABLE(s)
#   MultiColinear(test[,c(1,3)])
#   cl <- MultiColinear(test)
#
#   # PCA BIPLOT OF VARIABLES 
#    pca.test <- prcomp(test[,1:ncol(test)], scale=TRUE)
#    biplot(pca.test, arrow.len=0.1, xlabs=rep(".", length(pca.test$x[,1])))        
#
#   # REMOVE IDENTIFIED VARIABLE(S)
#   test <- test[, -which(names(test)==cl)]
#
# REFERENCES:
#  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. 
#     Wadsworth & Brooks/Cole. 
#
#  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978) 
#     LINPACK Users Guide. Philadelphia: SIAM Publications. 
#
# CONTACT: 
#     Jeffrey S. Evans, Ph.D.
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
MultiColinear <- function(x, p=1e-07) {
 if (!inherits(x, "data.frame")) stop("X MUST BE A data.frame")
   if ( (dim(x)[2] < 2) == TRUE) stop("NOT ENOUGH VARIABLES TO TEST")
     xtest <- x
     x.names <- names(xtest)
  qrx <- qr(xtest, tol=p)
    if (length(names(xtest)[qrx$pivot[1:qrx$rank]]) != length(xtest) )  
      {  
        keep <- names(xtest)[qrx$pivot[1:qrx$rank]]
         warning("MULTI-COLINEAR VARIABLES: ", paste(setdiff(x.names, keep), collapse = ","))
      return(paste(setdiff(x.names, keep)))
    } else { print(" NO MULTICOLINEAR VARIABLES IDENTIFIED")
  } 
}

##########################################################################
# PROGRAM: rf.modelSel (FOR CLASSIFICATION OR REGRESSION)
# USE: RANDOM FOREST MODEL SELECTION USING SCALED IMPORTANCE VALUES
# REQUIRES: DATAFRAME WITH Y-VAR AND X-VAR IN SEQUENTIAL COLS
#           R 2.13.0
#           randomForest 4.6-2
#
# ARGUMENTS: 
#       ydata      Y Data for model 
#       xdata      X Data for model
#       imp.scale  Type of scaling for importance values (mir or se), default is mir
#       r          Vector of importance percentiles to test i.e., c(0.1, 0.2, 0.5, 0.7, 0.9)
#       final      Run final model with selected variables (TRUE/FALSE)
#       plot.imp   Plot variable importance (TRUE/FALSE)
#       ...        Arguments to pass to randomForest (e.g., ntree=1000, 
#                  replace=TRUE, proximity=TRUE)
#
# VALUE:
#     A LIST WITH THE FOLLOWING OBJECTS
#         rf.final - FINAL RF MODEL (LIST OBJECT) USING SELECTED VARIABLES (IF final=TRUE)
#         SELVARS - LIST OF FINAL SELECTED VARIABLES
#         TEST - VALIDATION USED IN MODEL SELECTION
#         IMPORTANCE - IMPORTANCE VALUES FROM SELECTED MODEL
#         PARAMETERS - VARIABLES USED IN EACH TESTED MODEL
#
# NOTES: 
#        IF YOU WANT TO RUN CLASSIFICATION MAKE SURE Y IS A FACTOR
#        OTHERWISE RF RUNS IN REGRESSION MODE
#
#        The mir scale option perfroms a row standardization and the se option performs
#        normalizes using The “standard errors” of the permutation-based importance measure.
#        Both options result in a 0-1 range but se summs to 1.  
#                  mir = i/max(i)
#                  se = (i / se) / ( sum(i) / se) 
#
#  IMPORTANCE CANNONT BE FALSE AND IS SET IN THE FUNCTION, SO DO NOT USE IMPORTANCE FLAG
#        
#        For regression the model selection criteria is; largest %variation 
#        explained, smallest MSE, and fewest parameters. 
#         
#        For classification; Smallest OOB error, smallest maximum within 
#        class error, and fewest parameters. 
#
# REFERENCES:
#    Evans, J.S. and S.A. Cushman (2009) Gradient Modeling of Conifer Species 
#      Using Random Forest. Landscape Ecology 5:673-683.
#
#    Murphy M.A., J.S. Evans, and A.S. Storfer (2010) Quantify Bufo boreas 
#      connectivity in Yellowstone National Park with landscape genetics. 
#      Ecology 91:252-261
#
#    Evans J.S., M.A. Murphy, Z.A. Holden, S.A. Cushman (2011). Modeling species 
#      distribution and change using Random Forests CH.8 in Predictive Modeling in 
#      Landscape Ecology eds Drew, CA, Huettmann F, Wiersma Y. Springer 
# 
# EXAMPLES: 
# # CLASSIFICATION
#     data(iris)
#     iris$Species <- as.factor(iris$Species) 
#     ( rf.class <- rf.modelSel(iris[,1:4], iris[,"Species"], imp.scale="mir") ) 
#       vars <- rf.class$PARAMETERS[[3]]
#     ( rf.fit <- randomForest(x=iris[,vars], y=iris[,"Species"]) )                           
# # REGRESSION
#     data(airquality)
#     airquality <- na.omit(airquality)
#     ( rf.regress <- rf.modelSel(airquality[,2:6], airquality[,1], imp.scale="se") )
#       vars <- rf.regress$PARAMETERS[[3]]
#     ( rf.fit <- randomForest(x=airquality[,vars], y=airquality[,1]) )
#                               
# CONTACT: 
#     Jeffrey S. Evans, Ph.D.
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
rf.modelSel <- function(xdata, ydata, imp.scale="mir", r=c(0.25, 0.50, 0.75),  
                        final=FALSE, plot.imp=TRUE, ...) 
  {
 rf.ImpScale <- function (x, scale="mir") { 
  if (!inherits(x, "randomForest")) 
       stop(deparse(substitute(x)), " Must be a randomForest object")
  if (x$type == "regression") {
   if (is.null(x$importanceSD) == TRUE | "%IncMSE" %in% 
       names(as.data.frame(x$importance)) == FALSE)
        stop("OBJECT DOES NOT CONTAIN PERMUTATED IMPORTANCE, PLEASE RUN 
              randomForest WITH importance=TRUE")   
	rf.imp <- x$importance[,"%IncMSE"]
    rf.impSD <- x$importanceSD[,"%IncMSE"]
       rf.impSD[rf.impSD == 0] <- 0.000000001	
    if (scale == "mir") {
      i <- rf.imp / max(rf.imp) 
		}	  
    if (scale == "se") {
	  i <- ( rf.imp / rf.impSD ) / sum(rf.imp / rf.impSD, na.rm=TRUE)			 
        }
	 }
  if (x$type == "classification" | x$type == "unsupervised") {
   if (is.null(x$importanceSD) == TRUE | "MeanDecreaseAccuracy" %in% 
       names(as.data.frame(x$importance)) == FALSE)
        stop("OBJECT DOES NOT CONTAIN PERMUTATED IMPORTANCE, PLEASE RUN 
       randomForest WITH importance=TRUE") 
	rf.imp <- x$importance[,"MeanDecreaseAccuracy"]
    rf.impSD <- x$importanceSD[,"MeanDecreaseAccuracy"]
       rf.impSD[rf.impSD == 0] <- 0.000000001	
    if (scale == "mir") {
      i <- rf.imp / max(rf.imp) 
		}	  
    if (scale == "se") {
	  i <- ( rf.imp / rf.impSD ) / sum(rf.imp / rf.impSD, na.rm=TRUE)			 
        }
	 }
	i <- as.data.frame(i)
	  names(i) <- "importance" 
        row.names(i) <- names(rf.imp)	 
   return( i )            
 }
RFtype <- is.factor(ydata) #TEST FOR FACTOR IN Y 
##CLASSIFICATION##
if (RFtype == "TRUE") {
    model.vars <- list()
    ln <- 0
  rf.all <- randomForest(x=xdata, y=ydata, importance=TRUE, ...) 
      model.vars[[ln <- ln + 1]] <- rownames(rf.all$importance)  
       class.errors <- as.data.frame(rf.all$err.rate)
        class.errors <- na.omit(class.errors)  
         class.errors[class.errors == NaN] <- 0
          class.errors[class.errors == Inf] <- 1         
        i <- as.vector(array(0, dim=c((0),(1))))
	   for ( l in 2:nlevels(as.factor(names(class.errors))) )
          {
          x.bar <- mean(class.errors[,l])              
            i <- as.vector(append(i, x.bar, after=length(i) ))
            }        
         max.error = max(i[2:length(i)] ) 
	imp <- rf.ImpScale(rf.all, scale=imp.scale) 
    results <- as.data.frame(array(0, dim=c( 0, 4 )))
      x <- c(0, (median(rf.all$err.rate[,"OOB"]) * 100), max.error * 100, dim(xdata)[2] )
    results <- rbind(results, x) 	 	 
     for (p in 1:length(r) ) {
		 t = quantile(imp[,1], probs=r[p], na.rm=TRUE)
         sel.imp <- subset(imp, importance >= t)
           sel.vars <- rownames(sel.imp)
     if (length( sel.vars ) > 1) {                             
         xdata.sub <- xdata[,sel.vars]       
      rf.model <- randomForest(x=xdata.sub, y=ydata, importance=TRUE, ...)          
           class.errors <- as.data.frame(rf.model$err.rate)
            class.errors <- na.omit(class.errors)  
             class.errors[class.errors == NaN] <- 0
              class.errors[class.errors == Inf] <- 1      
        i <- as.vector(array(0, dim=c((0),(1))))
       for ( l in 2:nlevels(as.factor(names(class.errors))) )
          {
          x.bar <- mean(class.errors[,l])              
            i <- as.vector(append(i, x.bar, after=length(i) ))
            }        
         max.error = max(i[2:length(i)] )     
         x <- c(t, median(rf.model$err.rate[,1]) * 100, max.error * 100, length(sel.vars) )
         results <- rbind(results, x)
		  model.vars[[ln <- ln + 1]] <- rownames(rf.model$importance)
         }
        }
  names(results) <- c("THRESHOLD", "OOBERROR", "CLASS.ERROR", "NPARAMETERS")
  results <- results[order(results$CLASS.ERROR, results$OOBERROR, results$NPARAMETERS),]  
    t <- as.vector(results[,"THRESHOLD"])[1]     
        sel.imp <- subset(imp, importance >= t)    
        sel.vars <- rownames(sel.imp)	                              
  } # END OF CLASSIFICATION    
##REGRESSION## 
if (RFtype == "FALSE") {
    model.vars <- list()
      ln <- 0      
    rf.all <- randomForest(x=xdata, y=ydata, importance=TRUE, ...) 
	  model.vars[[ln <- ln + 1]] <- rownames(rf.all$importance)  
	   imp <- rf.ImpScale(rf.all, scale=imp.scale) 
      results <- as.data.frame(array(0, dim=c( 0, 4 )))
     x <- c(0, (median(rf.all$rsq)), mean(rf.all$mse), dim(xdata)[2] )
     results <- rbind(results, x)     
   for (p in 1:length(r) ) {
        t = quantile(imp[,1], probs=r[p], na.rm=TRUE)		 
        sel.vars <- rownames(subset(imp, importance >= t))  
     if (length( sel.vars ) > 1) {                             
      xdata.sub <- as.data.frame(xdata[,sel.vars]) 
      rf.model <- randomForest(x=xdata.sub, y=ydata, importance=TRUE, ...)          
      x <- c(t, (median(rf.model$rsq)), mean(rf.model$mse), length(sel.vars) )
      results <- rbind(results, x)
	     model.vars[[ln <- ln + 1]] <- rownames(rf.model$importance)
      }
    }
   names(results) <- c("THRESHOLD", "VAREXP", "MSE", "NPARAMETERS")
   results <- results[order(-results$VAREXP, results$MSE, results$NPARAMETERS),]  
   t <- as.vector(results[,"THRESHOLD"])[1] 
      sel.imp <- subset(imp, importance >= t)    
      sel.vars <- rownames(sel.imp)	  
   } # END OF REGRESSION 
    if (plot.imp == TRUE) { 
	  p <- as.matrix(subset(imp, importance >= t))    
       ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
       dotchart(p[ord,1], main="Model Improvment Ratio", pch=19)
    }
    if (final == TRUE) {
       sub.xdata <- xdata[,sel.vars]  #SUBSET VARIABLES
        rf.final <- randomForest(x=sub.xdata, y=ydata, importance=TRUE, ...)           
      ( list(MODEL=rf.final, SELVARS=sel.vars, TEST=results, IMPORTANCE=sel.imp, PARAMETERS=model.vars) )      
         } else {
      ( list(SELVARS=sel.vars, TEST=results, IMPORTANCE=sel.imp, PARAMETERS=model.vars) ) 
    }     
 }

######################################################################################
# PROGRAM: rf.ClassBalance (FOR CLASSIFICATION)
# USE: CREATES A BALANCED SAMPLE IN A RANDOM FORESTS CLASSIFICATION MODEL 
# REQUIRES: R 2.15.0, randomForest 4.6-5 (CURRENT TESTED VERSIONS) 
#                                                                                                                                                                                                     
# ARGUMENTS:  
#       ydata      RESPONSE VARIABLE  (i.e., [,2] or [,"SPP"] )                        
#       xdata      INDEPENDENT VARIABLES(S)  (i.e., [,3:14] or [3:ncol(data)] )
#       p          P-VALUE OF COVARIANCE CONVERGENCE TEST (DO NOT RECCOMEND CHANGING)
#       cbf        FACTOR USED TO TEST IF MODEL IS IMBALANCED, DEFAULT IS SIZE OF 
#                    MINORITY CLASS * 3                                              
#       ...        ADDITIONAL ARGUMENTS TO PASS TO RANDOM FOREST                          
# 
# VALUE:  
#      A LIST OBJECT WITH RF MODEL OBJECT, CUMMLIATIVE OOB ERROR,        
#        CUMMLIATIVE CONFUSION MATRIX, AND PERCENT CORRECT CLASSIFIED  
#
# NOTES:
#     RUNS MODEL WITH RANDOM SUBSET OF MAJORITY CLASS UNTIL                   
#       COVARIANCE CONVERGES TO FULL DATA. PREDECTION IS BASED ON                 
#       COMBINING SUBSET MODELS TO CREATE FINAL ENSEMBLE
#
# REFERENCES:
#    Evans, J.S. and S.A. Cushman (2009) Gradient Modeling of Conifer Species 
#      Using Random Forest. Landscape Ecology 5:673-683.
#
#    Evans J.S., M.A. Murphy, Z.A. Holden, S.A. Cushman (2011). Modeling species 
#      distribution and change using Random Forests CH.8 in Predictive Modeling in 
#      Landscape Ecology eds Drew, CA, Huettmann F, Wiersma Y. Springer 
#                                                                       
# EXAMPLES: 
#      rfClassBalance(ydata=data[,1], xdata=data[,3:ncol(data)], ntree=100)       
#                                                                                       
# CONTACT:
#     Jeffrey S. Evans
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
######################################################################################
rfClassBalance <- function (ydata, xdata, p=0.005, cbf=3,...) 
 {
 if (!require (randomForest)) stop("randomForest PACKAGE MISSING")
  if (  class(ydata) != "factor" ) { ydata <- as.factor(ydata) }
  CompCov <- function(m1, m2, pVal=p) {
       k = 2
        p = 2
         n1 = dim(m1)[1]
          n2 = dim(m2)[1] 
           n = n1 + n2
            s1 <- crossprod(m1[1:dim(m1)[1]])
             s2 <- crossprod(m2[1:dim(m2)[1]])
              c1 = (1/(n1-1))*s1
              c2 = (1/(n2-1))*s2
             c = (s1+s2)/(n-k)
            d = det(c)
            d1 = det(c1)
           d2 = det(c2) 
          m = ( (n-k)*log(d) ) - ( (n1-1)*log(d1) + (n2-1)*log(d2) )
         h = 1-((2*p*p+3*p-1) / (6*(p+1)*(k-1)) * (1 / (n1-1)+1 / (n2-1)+1 / (n-k)))
        chi = round(abs(m*h),digits=6)
       df = p*(p+1)*(k-1)/2
       print( paste("EQUIVALENCE p", chi, sep=": ") )
          if ( (chi <= pVal ) == TRUE & (i > 2) |  (i > 20)  == TRUE ) { 
               ( "TRUE" )
                  } else {
               ( "FALSE" ) 
             }
         }  		 
    y <- ydata
    x <- xdata  		 		 
    class.ct <- as.numeric()		 
      for(i in 1:nlevels(y)) {
        class.ct <- append(class.ct, length(which(y==levels(y)[i])), 
    	                   after=length(class.ct) )
        }
        maj.post <- which.max(class.ct)
    	  maj.class <- levels(ydata) [maj.post]
        min.post <- which.min(class.ct)
    	  min.class <- levels(ydata) [min.post]
    		
    if ( ( class.ct[maj.post] <= class.ct[min.post] * cbf ) == TRUE) 
            stop("CLASSES ARE BALANCED!")      
    tmp.data <- data.frame(y, x)
	   majority <- tmp.data[tmp.data[,"y"] == maj.class ,]       
       minority <- tmp.data[tmp.data[,"y"] == min.class ,]    
	   all.cov <- cov(majority[,names(x)])     
	  test <- as.data.frame(array(0, dim=c( 0, dim(tmp.data)[2] )))
        names(test) <- names(majority) 
     if ( !is.na(match("rf.model",ls()))) rm(rf.model)
        n <- dim(minority)[1]*2                 
  i=0  
    converge = c("FALSE")  
     while (converge != "TRUE" )
       {
       i=i+1
        ns <- sample(1:nrow(majority), n) 
        class.sample <- majority[ns, ]
          mdata <- rbind(minority, class.sample)   
        if (  class(mdata[,1]) != "factor" ) 
                   { mdata[,1] <- as.factor(mdata[,1]) }
        if ( !is.na(match("rf.model",ls()))) {               
            rf.fit <- randomForest(x=mdata[,2:ncol(mdata)], y=mdata[,1])                           
            rf.model <- combine(rf.fit, rf.model)           
               OOB <- ( OOB + median(rf.fit$err.rate[,1]) ) 
               CM <- (CM + rf.fit$confusion)                 
               confusion <- confusion + rf.fit$confusion 
               } else {
            rf.model <- randomForest(x=mdata[,2:ncol(mdata)], y=mdata[,1])  
               OOB <- median(rf.model$err.rate[,1]) 
               CM <- rf.model$confusion           
               confusion <- rf.model$confusion                          
         }
      test <- rbind(test, class.sample)    
         test.cov <- cov( test[,names(x)] )
         converge <- CompCov(all.cov, test.cov)  
  }
    OOB <- OOB / i
    CM[,3] <- CM[,3] / i
    PCC <- ( sum(diag(CM))/sum(CM) ) * 100
   list( MODEL=rf.model, OOB_ERROR=OOB, CONFUSION=CM, PCT_CC=PCC )
}
  
#######################################################################################
### PERCENT AREA SAMPLE OF POLYGONS 
psample <- cmpfun(function(x, pct=0.10, join=FALSE, msamp=5, sf=4046.86, stype="hexagonal", ...)
   {
     if (!inherits(x, "SpatialPolygonsDataFrame")) stop("MUST BE SP SpatialPolygonsDataFrame OBJECT")
    pids <- rownames(x@data)    
    psub <- x[rownames(x@data) == pids[1] ,]
	 ac <- sapply(slot(psub, 'polygons'), function(i) slot(i, 'area')) / sf 
      ns <- round((ac * pct), digits=0)
	    if(ns < msamp) {ns = msamp} 
	   psamp <- spsample(psub, n=ns, type=stype, ...)
      results <- SpatialPointsDataFrame(psamp, data=data.frame(ID=rep(as.numeric(pids[1]), 
	                                    dim(coordinates(psamp))[1]))) 
   if ( length(pids) > 1 ) { 
	for (i in 2:length(pids) ) 
      {    
       psub <- x[rownames(x@data) == pids[i] ,]
	    ac <- sapply(slot(psub, 'polygons'), function(i) slot(i, 'area')) / sf 
         ns <- round((ac * pct), digits=0)
	      if(ns < msamp) {ns = msamp} 
        psamp <- spsample(psub, n=ns, type=stype, ...)
       psamp <- SpatialPointsDataFrame(psamp, data=data.frame(ID=rep(as.numeric(pids[i]), 
	                                   dim(coordinates(psamp))[1])))
       results <- rbind(results, psamp) 	   
        }
	 }
   if( join == TRUE ) { 
      x@data <- cbind(xxID=as.numeric(as.character(rownames(x@data))), x@data)
        results@data <- data.frame(results@data, x@data[match(results@data$ID, x@data$xxID),]) 	  
	  x@data <- x@data[,-1]
	  return(results)
        } else {
    return( results )
  }
} )

##########################################################################
# FUNCTION TO CALCULATE PERCENT OF BINARY RASTER(S) WITHIN SPECIFIED WINDOW
brpct <- function(x, rasters, ...) {
   for(i in rasters) {
     r <- extract(i, x, ...)
       rlist <- lapply(r, FUN=function(x) {length(x[x == 1]) / length(x) } ) 
         rlist <- data.frame(unlist(rlist)) 
           names(rlist) <- paste(i) 
    if(exists(as.character(substitute(rdf))) == FALSE) { 
      rdf <- rlist 
        } else {
      rdf <- data.frame(rdf, rlist)
        }
    return( rdf )
  } 
}       

##########################################################################  
#### LOOP THROUGH X RASTERS AND ASSIGN VALUES TO SAMPLE POINTS
rvalues <- cmpfun(function(x, rasters, path, type="img") {
    if (!inherits(x, "SpatialPointsDataFrame")) stop("MUST BE SP SpatialPointsDataFrame OBJECT")
     results <- (x)
	   sc <- dim(x@data)[2]
    for (i in 1:length(rasters) ) {            
        r <- raster(paste(path, paste(rasters[i], type, sep="."), sep="/"))            
        v <- extract(r, results)
        v.df <- as.data.frame(v)
           v.df[is.na(v.df) == TRUE] <- 0
        results@data = data.frame(results@data, v.df[match(rownames(results@data), rownames(v.df)),])
          names(results@data)[i+sc] <- rasters[i]                      
     }
  return(results)
} )

##########################################################################
# PROGRAM: RasterUpSample.R
# USE: UP SAMPLES A RASTER USING ROBUST REGRESSION 
# REQUIRES: RGDAL FORMAT COMPATABLE RASTERS
#           PACKAGES: MASS, sp, raster, rgdal 
#
# ARGUMENTS: 
#  x                X (HIGHER RESOULTION) INDEPENDENT VARIABLE RASTER 
#  y                Y (LOWER RESOULTION) DEPENDENT VARIABLE RASTER    
#  p                PERCENT SUBSAMPLE
#  sample.type      TYPE OF SAMPLE (random OR systematic); DEFAULT IS random
#  file             IF SPECIFIED, A RASTER SURFACE WILL BE WRITTEN TO DISK.
#                     THE FILE EXTENTION WILL DICTATE THE RASTER FORMAT.
#  ...              ADDITIONAL ARGUMENTS PASSED TO predict
#
# EXAMPLES: 
#    setwd("C:/ANALYSIS/TEST/RRR")
#    x <- paste(getwd(), paste("elev", "img", sep="."), sep="/")
#    y <- paste(getwd(), paste("precip90", "img", sep="."), sep="/")
#    RasterUpSample(x=x, y=y, p=0.01, sample.type="random", filename="RPREDICT.img")
#      praster <- raster( paste(getwd(), "RPREDICT.img", sep="/"))
#      Y <- raster(paste(getwd(), paste("precip90", "img", sep="."), sep="/"))
#     op <- par(mfrow = c(1, 2))
#        plot(Y)
#        plot(praster) 
#     par(op)
#
# CONTACT: 
#     Jeffrey S. Evans, Ph.D.
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
RasterUpSample <- function(x, y, p, sample.type="random", filename=FALSE, ...) {
 	  X <- stack(x)
	  Y <- raster(y) 
     if(sample.type == "random") { 
	   print("SAMPLE TYPE RANDOM")
	    SubSamps <- sampleRandom(X, ((nrow(X)*ncol(X))*p), sp=TRUE)
		} 
	 if(sample.type == "systematic") {
       print("SAMPLE TYPE SYSTEMATIC")
      SubSamps <- sampleRegular(X, ((nrow(X)*ncol(X))*p), asRaster=TRUE)	 
      SubSamps <- as(SubSamps, 'SpatialPointsDataFrame') 
		}  	 
	  Yvalues <- extract(Y, SubSamps)
    SubSamps@data <- data.frame(SubSamps@data, Y=Yvalues)
       SubSamps@data <- na.omit(SubSamps@data)	
   ( rrr <- rlm(as.formula(paste(names(SubSamps@data)[2], ".", sep=" ~ ")), 
                data=SubSamps@data, scale.est="Huber", psi=psi.hampel, 
                init="lts") )
  if (filename != FALSE) {
  	predict(X, rrr, filename=filename, na.rm=TRUE, progress="window", 
	        overwrite=TRUE, ...)
     print(paste("RASTER WRITTEN TO", filename, sep=": "))			
	}
     #print(paste("MEAN RESIDUAL ERROR", round(mean(rrr$residuals), digits=5), sep=": "))
     #print(paste("AIC", round(AIC(rrr), digits=5), sep=": "))   
  return(rrr)		
}

##########################################################################
validation <- function(predictedValues, actualValues, cutoff="automatic", cutoffValue=NULL, 
                    colorize=TRUE, .title=NULL, summaryFile=NULL, bg="white")
{
    require(ROCR)
    pred <- prediction(predictedValues, actualValues)
    
    # Create the ROC performance object. 
    perf <- performance(pred, "tpr", "fpr")
    
    # Calculate model summary statistics.
    nLabel <- min(actualValues)                   
    pLabel <- max(actualValues)                   
    
    auc <- performance(pred, "auc")@y.values[[1]]
    
    if ((nLabel == 0 || nlabel == 1) && (pLabel == 0 || pLabel == 1))
        mxe <- performance(pred, "mxe")@y.values[[1]]
    else
        mxe <- NA
        
    prbe <- unlist(performance(pred, "prbe")@y.values)
    if (length(prbe) > 0)
        prbe <- prbe[length(prbe)]
    else
        prbe <- NA
        
    if (is.numeric(nLabel))
        rmse <- performance(pred, "rmse")@y.values[[1]]
    else
        rmse <- NA

    messages = vector(mode="character")

    messages = append(messages, "Model summary statistics:")
    messages = append(messages, "")
    messages = append(messages, sprintf("Area under the ROC curve (auc)           = %f", auc))
    if (!is.na(mxe))
        messages = append(messages, sprintf("Mean cross-entropy (mxe)                 = %f", mxe))
    messages = append(messages, sprintf("Precision-recall break-even point (prbe) = %f", prbe))
    messages = append(messages, sprintf("Root-mean square error (rmse)            = %f", rmse))
    
    # If asked us to calculate the cutoff, calculate it.
    if (cutoff == "automatic")
    {
        distFromPerfect <- sqrt(unlist(perf@x.values)^2 + (1 - unlist(perf@y.values))^2)
        cutoffValue <- unlist(perf@alpha.values)[which.min(distFromPerfect)]
    }
    
    # If there is a cutoff, calculate the contingency table. 
    if (cutoff != "none")
    {
        tn = length(which((predictedValues < cutoffValue) & (actualValues == nLabel)))
        fn = length(which((predictedValues < cutoffValue) & (actualValues != nLabel)))
        tp = length(which((predictedValues >= cutoffValue) & (actualValues == pLabel)))
        fp = length(which((predictedValues >= cutoffValue) & (actualValues != pLabel)))
        
        messages = append(messages, "")
        messages = append(messages, sprintf("Contingency table for cutoff = %f:", cutoffValue))
        messages = append(messages, "")
        messages = append(messages, "             Actual P  Actual N     Total")
        messages = append(messages, sprintf("Predicted P %9i %9i %9i", tp, fp, tp+fp))
        messages = append(messages, sprintf("Predicted N %9i %9i %9i", fn, tn, tn+fn))
        messages = append(messages, sprintf("      Total %9i %9i %9i", tp+fn, fp+tn, tp+fn+fp+tn))
        messages = append(messages, "")

        tn = as.double(tn)
        fn = as.double(fn)
        tp = as.double(tp)
        fp = as.double(fp)
        acc = (tp+tn)/(tp+fp+tn+fn)

        messages = append(messages, sprintf("Accuracy (acc)                                = %f", acc))
        messages = append(messages, sprintf("Error rate (err)                              = %f", (fp+fn)/(tp+fp+tn+fn)))
        messages = append(messages, sprintf("Rate of positive predictions (rpp)            = %f", (tp+fp)/(tp+fp+tn+fn)))
        messages = append(messages, sprintf("Rate of negative predictions (rnp)            = %f", (tn+fn)/(tp+fp+tn+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("True positive rate (tpr, or sensitivity)      = %f", tp/(tp+fn)))
        messages = append(messages, sprintf("False positive rate (fpr, or fallout)         = %f", fp/(fp+tn)))
        messages = append(messages, sprintf("True negative rate (tnr, or specificity)      = %f", tn/(fp+tn)))
        messages = append(messages, sprintf("False negative rate (fnr, or miss)            = %f", fn/(tp+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("Positive prediction value (ppv, or precision) = %f", tp/(tp+fp)))
        messages = append(messages, sprintf("Negative prediction value (npv)               = %f", tn/(tn+fn)))
        messages = append(messages, sprintf("Prediction-conditioned fallout (pcfall)       = %f", fp/(tp+fp)))
        messages = append(messages, sprintf("Prediction-conditioned miss (pcmiss)          = %f", fn/(tn+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("Matthews correlation coefficient (mcc)        = %f", (tp*tn - fp*fn)/sqrt((tp+fn)*(fp+tn)*(tp+fp)*(fn+tn))))
        messages = append(messages, sprintf("Odds ratio (odds)                             = %f", (tp*tn)/(fn*fp)))
        messages = append(messages, sprintf("SAR                                           = %f", (acc + auc + rmse)/3))
    }
    else
    {
        cutoffValue = NA
        tp = NA
        fp = NA
        tn = NA
        fn = NA
    }
    
    # Output the messages, optionally to the summaryFile.
    writeLines("")
    writeLines(messages)
    writeLines("")
    
    if (!is.null(summaryFile))
        writeLines(messages, summaryFile)
    
    # Create the ROC plot.
    plot(perf, colorize=colorize, lwd=5, main=.title)
    abline(0, 1, lty=2)
    if (cutoff != "none")
    {
        tpr = tp/(tp+fn)
        fpr = fp/(fp+tn)
        if (colorize)
            points(x=fpr, y=tpr, cex=1.5, lwd=2)
        else
            points(x=fpr, y=tpr, pch=21, cex=1.5, lwd=2, bg=bg)
        text(x=fpr+0.01, y=tpr-0.01, labels=sprintf("Cutoff = %f", cutoffValue), adj=c(0,1))
    }
    
    # Return successfully.
    return(c(cutoffValue, tp, fp, fn, tn, auc, mxe, prbe, rmse))
}
