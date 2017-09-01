# Usage: source("tace_v2.R")
#     -or- 
# Usage: Rscript tace_v2.R
# 
# dependencies:  
#   install.packages("leaps",repos='http://cran.us.r-project.org')
#   install.packages("caret",repos='http://cran.us.r-project.org')
#   install.packages("GGally",repos='http://cran.us.r-project.org')
#   install.packages("maptree",repos='http://cran.us.r-project.org')
#   install.packages("RANN",repos='http://cran.us.r-project.org')
#   install.packages("tree",repos='http://cran.us.r-project.org')
#   install.packages("ranger",repos='http://cran.us.r-project.org')
#   install.packages("subselect",repos='http://cran.us.r-project.org')
#   install.packages("Boruta",repos='http://cran.us.r-project.org')
# 
options("width"=180)
rm(list=ls())
# Load dependencies
libs <- c("randomForest", "e1071", "xgboost", "leaps", "MASS", "ranger", 
    "caret", "cluster", "subselect", "Boruta")
lapply(libs, library,  character.only = TRUE)

# Parameters/Load Data
dataset <- read.csv(paste0("file:///home/gpauloski/git-repos/TACE/",
    "gmmdatamatrixfixed22Aug2017_Greg_edited.csv"))
volumes <- TRUE
stepwise <- FALSE   
exhaustive <- FALSE 
anneal <- FALSE 
genetic <- TRUE 
boruta <- TRUE 
semisupervised <- TRUE    # Perform semisupervised learning 
kClusters <- 9            # Number of clusters in SSL (Cluster 1 is skipped)
outputFile <- "model_predictions.csv"  # file save predictions of each model

# Set target columns and convert binary target to factor
ttpTarget <- "liver_TTP"
targets <- c("split21", "split28", "random")
dataset <- cbind(
    split21 = as.factor(ifelse(dataset[,ttpTarget] < 21, 1, 2)), 
    split28 = as.factor(ifelse(dataset[,ttpTarget] < 28, 1, 2)), 
    random = as.factor(round(runif(nrow(dataset), 1, 2))), dataset)

# Set variable columns and convert strings to numeric 
varMain <- c( "liver_TNM","liver_BCLC", "liver_CLIP", "liver_Okuda")
for(i in 1:length(varMain)) {
  dataset[,varMain[i]] <- as.numeric(factor(dataset[,varMain[i]], 
                                            levels=levels(factor(dataset[,varMain[i]])) 
                                           ))
}


# Create list of img data subsets
# @gpauloski null model should be without imaging
## NULL Model is without image features Selection ##
# https://stats.stackexchange.com/questions/259636/what-is-null-model-in-regression-and-how-does-it-related-to-null-hypothesis
# https://stackoverflow.com/questions/24225451/how-to-create-a-null-model-for-conditional-logistic-regression-in-r
varImg <- list("nullModel"=NULL,"volumes" = NULL, "stepwise" = NULL, "exhaustive" = NULL,
    "anneal" = NULL, "genetic" = NULL, "boruta" = NULL)

# Filter only baseline obs
dataset <- dataset[which(dataset[,"liver_TimeID"] == "baseline"),]

# Get list of col names for image data and remove empty columns
imgData <- NULL
imgDataInd <- seq(which(colnames(dataset) == "liver_Volume"), ncol(dataset),1)
for(i in 1:length(imgDataInd)) {
  if(length(which(is.na(dataset[,imgDataInd[i]]))) < 10) {
    imgData <- c(imgData, colnames(dataset)[imgDataInd[i]])
  }
}

# Create img data list with high corr vars removed. To be used in subsets
# Pre process data using knnimpute from caret package
dataPreProcess <-preProcess(dataset[,c(ttpTarget,imgData)], 
    method = c("knnImpute"))
# Impute data using preProcess to get new dataProcess w/ no missing vals
dataProcess <- predict(dataPreProcess,dataset[,c(ttpTarget,imgData)])
# Find variables with corr coef > 0.8
highCor <- findCorrelation(cor(dataProcess), cutoff = 0.8)
# Make new image data vector with high corr variables removed from imgData
imgDataClean <- setdiff(imgData, colnames(dataProcess)[highCor])
dataProcess <- dataProcess[,-highCor]


## Volume Selection ##
if(volumes) {
  varImg$volumes <- c("liver_Volume", "necrosis_Volume", "viable_Volume", 
      "viable_Art_DENOISE")
  print("Volume subset selection finished")
}

## STEPWISE Selection ##
# Stepwise AIC fails if num vars > num obs. If you get error that AIC is
# infinity, decrease the correlation coefficient cutoff. This will increase
# the number of corr vars removed from the stepwise regression
if(stepwise) {
  # build linear model of dataset
  lm <- lm(dataProcess[,ttpTarget]~.,data=dataProcess[,imgDataClean], 
      singular.ok=TRUE)
  # Perform forward stepwise regression on linear model 
  stpReg <- stepAIC(lm,direction="both",trace=FALSE)
  # Get best subset from regression output
  coefs <- summary(stpReg)$coefficients[,4]  
  # Add best subset to stepwise object in varImg
  varImg$stepwise <- names(coefs)[which(coefs < 0.15)]
  print("Stepwise subset selection finished")
  print (varImg$stepwise)
}

## EXHAUSTIVE Selection ##
if(exhaustive) {
  # run regsubsets to get best subset of 8 vars (8 b/c it takes a while)
  reg <- regsubsets(x=dataProcess[,imgData],y=dataProcess[,ttpTarget],
      method="exhaustive",really.big=T,nvmax=8)
  # Get names of vars in best subset
  fits <- coef(reg,8)
  # Add best subset to exhaustive object in varImg
  varImg$exhaustive <- names(fits)[-1]
  print("Exhaustive subset selection finished")
  print (varImg$exhaustive )
}

## ANNEAL Selection ##
if(anneal) {
  # Default to 8 variables selected in best model
  # Anneal can be highly tuned, may be wise to test it out more
  ann <- anneal(cor(dataset[,imgDataClean]),8)
  varImg$anneal <- names(dataset[,imgDataClean])[ann$bestsets]
  print("Annealing subset selection finished")
  print (varImg$anneal)
}

## GENETIC Selection ##
if(genetic) {
  gen <- genetic(cor(dataset[,imgDataClean]),8)
  varImg$genetic <- names(dataset[imgDataClean])[gen$bestsets]
  print("Genetic subset selection finished")
  print (varImg$genetic)
}

## BORUTA Selection ##
if(boruta) {
  bor <- Boruta(x=dataset[,imgData], y=dataset[,ttpTarget], pValue=0.35)
  varImg$boruta <- names(dataset[,imgData])[
      which(bor$finalDecision == "Confirmed")]
  print("Boruta subset selection finished")
  print (varImg$boruta)
}

# Returns vector of error matrix values and percent error
# p = predicted value from model, a = actual value
# only works on binary classifications 1 and 2
errorMatrix <- function(p, a) {
  return(c(length(which(p == 1 & a == 1)), length(which(p == 1 & a == 2)),
      length(which(p == 2 & a == 1)), length(which(p == 2 & a == 2)),
      length(which(p != a)) / length(p)))
}

# Variables to store results/errors of models
pred <- data.frame(obs = seq(1,nrow(dataset),1))
predErrors <- NULL
predErrorNames = c("modelID", "p0a0", "p0a1", "p1a0", "p1a1", "errors")

# Number of iterations. If semisupervised = false, iters = 1 and the script
# will only perform supervised learning. If semisupervised = true, iters =
# kClusters meaning run 1 will be supervised and the following runs will be 
# semi-supervised with k = 2:kClusters
iters <- ifelse(semisupervised, kClusters, 1)

# Loop for each target variable
for(h in 1:length(targets)) {

  # Loop for each of 4 baseline vars
  for(i in 1:length(varMain)) {
  
    # Loop for each set of imgData
    for(j in 1:length(varImg)) {

        # Loop for each in iters. See iters variable for more info
        for(k in 1:iters) {
          cat("Building Models: target=", targets[h], ", input=[", varMain[i],
              ", ", names(varImg)[j], "] k=",k, sep="")
          # Create copy of dataset to work in; prevents overwrites
          ds <- dataset
          # Add new columns to pred data frame to store results
          pred <- cbind(pred, a = vector(length=nrow(ds)),
              b = vector(length=nrow(ds)), c = vector(length=nrow(ds)),
              d = vector(length=nrow(ds)))
          # Create name of column using var inputs and ML algorithm
          rfName <- paste0(targets[h], "_", varMain[i], "_", 
              names(varImg)[j], "_", "rf")
          rfWName <- paste0(targets[h], "_", varMain[i], "_", 
              names(varImg)[j], "_", "rfWeighted")
          svmName <- paste0(targets[h], "_", varMain[i], "_", 
              names(varImg)[j], "_", "svm")
          xgbName <- paste0(targets[h], "_", varMain[i], "_", 
              names(varImg)[j], "_", "xgb")

          # error check
          if( is.null(varImg[[j]]) & (names(varImg)[j] != "nullModel") ) {
            cat("skip me\n" , sep="")
            break
          # If k = 1, perform supervised learning
          } else if(k == 1) {
            cat(", Supervised: ",  sep="")
            # Build input var list by combining baseline and imgData vars
            input <- c(varMain[[i]], varImg[[j]])
            # Set colnames of data frame to these names
            colnames(pred)[colnames(pred)=="a"] <- rfName
            colnames(pred)[colnames(pred)=="b"] <- rfWName
            colnames(pred)[colnames(pred)=="c"] <- svmName
            colnames(pred)[colnames(pred)=="d"] <- xgbName
            alwaysTry <- varMain[[i]]

          # Else k != 1, perfrom semi-supervised learning
          # Only run clustering if  img data !null
          } else if ( !is.null(varImg[[j]])) {
            cat(", Semi-super (k=", k, "): ", sep="")
            rfName <- paste0(rfName, "_k", k)
            rfWName <- paste0(rfWName, "_k", k)
            svmName <- paste0(svmName, "_k", k)
            xgbName <- paste0(xgbName, "_k", k)
            # Set colnames of data frame to these names
            colnames(pred)[colnames(pred)=="a"] <- rfName
            colnames(pred)[colnames(pred)=="b"] <- rfWName
            colnames(pred)[colnames(pred)=="c"] <- svmName
            colnames(pred)[colnames(pred)=="d"] <- xgbName
            # Unsupervised random forest and add classifications to dataset
            rfUL <- randomForest(x=ds[,c(varMain[[i]], varImg[[j]])], 
                ntree=1001, replace=FALSE, mtry=length(c(varMain[[i]], 
                varImg[[j]])), na.action=na.roughfix)
            ds <- cbind(ds, clusters = pam(1-rfUL$proximity, k=k, diss=TRUE, 
                cluster.only = TRUE))
            # Build input var list by combining baseline and imgData vars
            input <- c(varMain[[i]], varImg[[j]], "clusters")
            alwaysTry <- c(varMain[[i]], "clusters")
          # error check
          } else  {
            cat("skip me\n" , sep="")
            break
          } 


          # For each obs in dataset, leave one out and train model on
          # remaining obs in dataset then use obs that was left out as test obs
          for(m in 1:nrow(ds)) {
            # Create vector of training obs missing one variable
            train <- seq(1,nrow(ds),1)
            train <- train[-m]

            ## RANDOM FOREST ##
            #cat(", RF ", sep="")
            modelFormula <- as.formula( paste0(targets[h], " ~ . ") )
            rf <- randomForest(modelFormula, data=ds[train,c(input,targets[h])], ntrees=1001,  replace=FALSE, na.action=na.roughfix)
            predInput = ds[m, input]
            ## Error check for one row ##
            if (!is.data.frame(predInput ) ) {
              predInput = as.data.frame(ds[m, input])
              colnames(predInput )<- input
            }
            pred[m,rfName] <- predict(rf, predInput )  

            ## WEIGHTED RANDOM FOREST ##
            #cat(", WRF ", sep="")
            if(!(is.null(varImg[[j]]))) {
            rfW <- ranger(modelFormula,  data=ds[train,c(input,targets[h])], num.trees=1001, always.split.variable=alwaysTry)
            } else {
            rfW <- ranger(modelFormula,  data=ds[train,c(input,targets[h])], num.trees=1001 )
            } 
            pred[m, rfWName] <- predict(rfW, predInput )$predictions


            ## SVM ##
            #cat(", SVM ", sep="")
            svm <- svm(x=as.matrix(ds[train,input]),
                y=ds[train,targets[h]], kernel="polynomial", degree=3)
            if(!(any(which(is.na(ds[m,input]))) )) {
              pred[m,svmName] <- predict(svm, predInput )
            } else pred[m,svmName] <- "NA"			

            ## XGBOOST ##
            #cat(", XGB ", sep="")
            trnList <- list("data" = as.matrix(ds[train,input]), 
                "label" = ifelse(ds[train,targets[h]] == 1, 0, 1))
            tstList <- list("data" = as.matrix(ds[m,input]), 
                "label" = ifelse(ds[m,targets[h]] == 1, 0, 1))
            bst <- xgboost(trnList$data, label=trnList$label, nrounds=501, 
                 objective="binary:logistic", verbose=0)
            pred[m,xgbName] <- ifelse(round(predict(bst, 
                tstList$data)) == 0, 1, 2)

          }

          # Calculate errors and error matrix
          cat(", pred errors \n ", sep="")
          predErrors <- rbind(predErrors, 
              c(rfName,  errorMatrix(pred[,rfName],  ds[,targets[h]]) ),
              c(rfWName, errorMatrix(pred[,rfWName], ds[,targets[h]]) ),
              c(svmName, errorMatrix(pred[,svmName], ds[,targets[h]]) ),
              c(xgbName, errorMatrix(pred[,xgbName], ds[,targets[h]]) ))

        } # end loop iters
    } # end loop varImg
  } # end loop varMain
} # end loop targets

# Transpose pred matrix and remove index vector
predTranspose <- data.frame(t(pred[,2:ncol(pred)]))
# Clean up colnames and modelIDs
for(i in 1:ncol(predTranspose)) colnames(predTranspose)[i] <- paste0("obs_", i) 

# Convert predErrors to data frame and give it column names
predErrors <- data.frame(predErrors)
colnames(predErrors) <- predErrorNames
# Combine predErrors and pred
predErrors <- cbind(predErrors, rank = rank(predErrors$error))

# SQL-type join on datamatrix
# @gpauloski: useful sql like commands to avoid indexing problems - https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
# STYLE: @gpauloski - SQL is your friend
# use modelID as  row key to id for join -  https://www.mongodb.com/nosql-explained
predMerge <- merge( y=predTranspose, x=predErrors ,by.y="row.names", by.x="modelID" )

# Write predErrors to csv
write.csv(predMerge , file=  outputFile, row.names = FALSE)
cat("\nResults saved in", outputFile, "\n")
