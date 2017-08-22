library(randomForest, quietly=TRUE)  # RF
library(e1071, quietly=TRUE)         # SVM
library(xgboost, quietly=TRUE)       # XGboost
library(leaps, quietly=TRUE)         # Reg subset selection
library(MASS, quietly=TRUE)          # Stepwise reg subset selection
library(caret, quietly=TRUE)         # Caret for findCorrelation
library(cluster, quietly=TRUE)       # pam function for clustering data
source("utils.R")                    # RankCor function used to rank vars
source("plotTree.R")

# Parameters/Load Data
dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrixfixed22Aug2017_Greg_edited.csv")
stepwise <- TRUE     # If TRUE: perform stepwise model selection
exhaustive <- TRUE  # If TRUE: perform exhaustive model selection
SSL <- TRUE          # Repeat each model with semi-supervised clusters added
kClusters <- 10      # Number of clusters in SSL
outputFile <- "model_predictions"  # file save predictions of each model

# Set target columns and convert binary target to factor
ttpTarget <- "liver_TTP"
targets <- c("targetSplit21", "targetSplit28", "targetRandom")
dataset <- cbind(
    targetSplit21 = as.factor(ifelse(dataset[,ttpTarget] < 21, 1, 2)), 
    targetSplit28 = as.factor(ifelse(dataset[,ttpTarget] < 28, 1, 2)), 
    targetRandom = as.factor(round(runif(nrow(dataset), 1, 2))), dataset)

# Create new target for xgboost b/c it can only use binary num vector of
# ones and zeros and add that to dataset
xgbTargets <- c("xgbTarget1", "xgbTarget2", "xgbTarget2")
dataset <- cbind(
    xgbTarget1 = ifelse(dataset[,targets[1]] == 1, 0, 1),
    xgbTarget2 = ifelse(dataset[,targets[2]] == 1, 0, 1),
    xgbTarget3 = ifelse(dataset[,targets[3]] == 1, 0, 1),  dataset)

# Set variable columns and convert strings to numeric 
# (b/c XGB can only use numeric). I.e. xgboost can't use factors so convert
# string factors to numeric
varMain <- c("liver_BCLC", "liver_CLIP", "liver_Okuda", "liver_TNM")
for(i in 1:length(varMain)) {
  dataset[,varMain[i]] <- as.numeric(factor(dataset[,varMain[i]],
      levels=levels(factor(dataset[,varMain[i]]))))
}

# Set names of volume group
varImg <- list("volumes" = NULL, "stepwise" = NULL, "exhaustive" = NULL)
varImg$volumes <- c("liver_Volume", "necrosis_Volume", "viable_Volume", 
    "viable_Art_DENOISE")

# Filter only baseline obs
dataset <- dataset[which(dataset[,"liver_TimeID"] == "baseline"),]

# Get list of col names for image data and remove empty columns
imgData <- NULL
imgDataIndex <- seq(which(colnames(dataset)==varImg$volumes[1]),ncol(dataset),1)
for(i in 1:length(imgDataIndex)) {
  if(length(which(is.na(dataset[,imgDataIndex[i]]))) < 10) {
    imgData <- c(imgData, colnames(dataset)[imgDataIndex[i]])
  }
}

## STEPWISE Selection ##
# Stepwise AIC fails if num vars > num obs. If you get error that AIC is
# infinity, decrease the correlation coefficient cutoff. This will increase
# the number of corr vars removed from the stepwise regression
if(stepwise) {
  # Pre process data using knnimpute from caret package
  dataPreProcess <-preProcess(dataset[,c(ttpTarget,imgData)], 
      method = c("knnImpute"))
  # Impute data using preProcess to get new dataProcess w/ no missing vals
  dataProcess <- predict(dataPreProcess,dataset[,c(ttpTarget,imgData)])
  # Get correlation coefs of dataset
  temp <- cor(dataProcess)
  # Find variables with corr coef > 0.8
  highCor <- findCorrelation(temp, cutoff = 0.8)
  # Make new image data vector with high corr variables removed from imgData
  imgDataStepwise <- setdiff(imgData, colnames(dataProcess)[highCor])
  # Remove highCor vars from dataset and create new dataset 
  dataProcess2 <- dataProcess[,-highCor]
  # build linear model of dataset
  lm <- lm(dataProcess2[,ttpTarget]~.,data=dataProcess2[,imgDataStepwise], 
      singular.ok=TRUE)
  # Perform forward stepwise regression on linear model 
  stpReg <- stepAIC(lm,direction="both",trace=FALSE)
  # Get best subset from regression output
  coefs <- summary(stpReg)$coefficients[,4]  
  # Add best subset to stepwise object in varImg
  varImg[[2]] <- names(coefs)[which(coefs < 0.15)]
}

## EXHAUSTIVE Selection ##
if(exhaustive) {
  # run regsubsets to get best subset of 8 vars (8 b/c it takes a while)
  reg <- regsubsets(x=dataProcess[,imgData],y=dataProcess[,ttpTarget],
      method="exhaustive",really.big=T,nvmax=8)
  # Get names of vars in best subset
  fits <- coef(reg,8)
  # Add best subset to exhaustive object in varImg
  varImg[[3]] <- names(fits)[-1]
}

# Create data frame to store predictions for each model and errors
pred <- data.frame(obs = seq(1,nrow(dataset),1))
predErrors <- NULL
predErrorNames = c("modelID", "p0a0", "p0a1", "p1a0", "p1a1", "errors")

# Loop for each target variable
for(h in 1:length(targets)) {
  # Set current binary target as well as xgb target
  binTarget = targets[h]
  binNumTarget = xgbTargets[h]

  # Loop for each of 4 baseline vars
  for(i in 1:length(varMain)) {
  
    # Loop for each set of imgData
    for(j in 1:length(varImg)) {

      # Only build models if list of img data !null
      if(!(is.null(varImg[[j]]))) {
        cat("Building model: input = [", varMain[i], ", ",  names(varImg)[j], 
            "], target = ", targets[h], "\n", sep="")

        # Build input var list by combining baseline and imgData vars
        input <- c(varMain[i], varImg[[j]])

        # Add new columns to pred data frame to store results
        pred <- cbind(pred, a = vector(length=nrow(dataset)),
            b = vector(length=nrow(dataset)), c = vector(length=nrow(dataset)))
        # Create name of column using var inputs and ML algorithm
        rfName <- paste0(targets[h], "_", varMain[i], "_", names(varImg)[j], 
            "_", "rf")
        svmName <- paste0(targets[h], "_", varMain[i], "_", names(varImg)[j], 
            "_", "svm")
        xgbName <- paste0(targets[h], "_", varMain[i], "_", names(varImg)[j], 
            "_", "xgb")
        # Set colnames of data frame to these names
        colnames(pred)[colnames(pred)=="a"] <- rfName
        colnames(pred)[colnames(pred)=="b"] <- svmName
        colnames(pred)[colnames(pred)=="c"] <- xgbName

        # For each obs in dataset, leave one out and train model on
        # remaining obs in dataset then use obs that was left out as test obs
        for(k in 1:nrow(dataset)) {
          # Create vector of training obs missing one variable
          train <- seq(1,nrow(dataset),1)
          train <- train[-k]

          ## RANDOM FOREST ##
          rf <- randomForest(dataset[train,binTarget]~.,
              data=dataset[train,input], ntrees=10000, mtry=length(input),
              replace=FALSE, na.action=na.roughfix)
          pred[k,rfName] <- predict(rf, dataset[k, input])

          ## SVM ##
          svm <- svm(x=as.matrix(dataset[train,input]),
              y=dataset[train,binTarget], kernel="polynomial", degree=3)
          if(!(any(which(is.na(dataset[k,input]))))) {
            pred[k,svmName] <- predict(svm, dataset[k,input])
          } else pred[k,svmName] <- "NA"			

          ## XGBOOST ##
          trnList <- list("data" = as.matrix(dataset[train,input]), 
              "label" = dataset[train,binNumTarget])
          tstList <- list("data" = as.matrix(dataset[k,input]), 
              "label" = dataset[k,binNumTarget])
          bst <- xgboost(trnList$data, label=trnList$label, nrounds=2, 
               objective="binary:logistic", verbose=0)
          pred[k,xgbName] <- ifelse(round(predict(bst, 
              tstList$data)) == 0, 1, 2)
        }

        # Calculate errors and error matrix
        predErrors <- rbind(predErrors, c(rfName, as.character(c( 
            length(which(pred[,rfName] == 1 & dataset[,binTarget] == 1)),
            length(which(pred[,rfName] == 1 & dataset[,binTarget] == 2)),
            length(which(pred[,rfName] == 2 & dataset[,binTarget] == 1)),
            length(which(pred[,rfName] == 2 & dataset[,binTarget] == 2)),
            length(which(pred[,rfName] != dataset[,binTarget])) / 
            length(pred[,rfName])))))
        predErrors <- rbind(predErrors, c(svmName, as.character(c(
            length(which(pred[,svmName] == 1 & dataset[,binTarget] == 1)),
            length(which(pred[,svmName] == 1 & dataset[,binTarget] == 2)),
            length(which(pred[,svmName] == 2 & dataset[,binTarget] == 1)),
            length(which(pred[,svmName] == 2 & dataset[,binTarget] == 2)),
            length(which(pred[,svmName] != dataset[,binTarget])) / 
            length(pred[,svmName]))))) 
        predErrors <- rbind(predErrors, c(xgbName, as.character(c( 
            length(which(pred[,xgbName] == 1 & dataset[,binTarget] == 1)),
            length(which(pred[,xgbName] == 1 & dataset[,binTarget] == 2)),
            length(which(pred[,xgbName] == 2 & dataset[,binTarget] == 1)),
            length(which(pred[,xgbName] == 2 & dataset[,binTarget] == 2)),
            length(which(pred[,xgbName] != dataset[,binTarget])) / 
            length(pred[,xgbName])))))

        # If SSL = TRUE
        if(SSL) {

          # Loop for number of clusters to try
          for(m in 1:kClusters) {

            # Add new columns to pred data frame to store results
            pred <- cbind(pred, a = vector(length=nrow(dataset)),
                b = vector(length=nrow(dataset)), 
                c = vector(length=nrow(dataset)))
            # Create name of column using var inputs and ML algorithm
            rfSSLName <- paste0(rfName, "_SSL_k", m)
            svmSSLName <- paste0(svmName, "_SSL_k", m)
            xgbSSLName <- paste0(xgbName, "_SSL_k", m)
            # Set colnames of data frame to these names
            colnames(pred)[colnames(pred)=="a"] <- rfSSLName
            colnames(pred)[colnames(pred)=="b"] <- svmSSLName
            colnames(pred)[colnames(pred)=="c"] <- xgbSSLName

            rfUL <- randomForest(x=dataset[,input], ntree=1000, 
                replace=FALSE, mtry=length(input), na.action=na.roughfix)
            clusters <- pam(1-rfUL$proximity, k=m, diss=TRUE)

            # Create new data frame and input list with clusters added
            ds <- cbind(dataset, clusters = clusters$clustering)
            inputSSL <- c(input, "clusters")

            # Leave one out
            for(k in 1:nrow(ds)) {

              ## RANDOM FOREST SSL
              rf <- randomForest(ds[train, binTarget]~., 
                   data=ds[train, inputSSL], ntrees=1000, replace=FALSE, 
                   mtry=length(inputSSL))           
              pred[k,rfSSLName] <- predict(rf, ds[k, inputSSL])

              ## SVM ##
              svm <- svm(x=as.matrix(ds[train,inputSSL]),
                  y=ds[train,binTarget], kernal="linear")
              if(!(any(which(is.na(ds[k,inputSSL]))))) {
                pred[k,svmSSLName] <- predict(svm, ds[k,inputSSL])
              } else pred[k,svmName] <- "NA"			

              ## XGBOOST ##
              trnList <- list("data" = as.matrix(ds[train,inputSSL]), 
                  "label" = ds[train,binNumTarget])
              tstList <- list("data" = as.matrix(ds[k,inputSSL]), 
                  "label" = ds[k,binNumTarget])
              bst <- xgboost(trnList$data, label=trnList$label, nrounds=2, 
                  objective="binary:logistic", verbose=0)
              pred[k,xgbSSLName] <- ifelse(round(predict(bst, 
                  tstList$data)) == 0, 1, 2)
            }

            # Calculate error and error matrix on SSL models        
            predErrors <- rbind(predErrors, c(rfSSLName, as.character(c( 
                length(which(pred[,rfSSLName] == 1 & dataset[,binTarget] == 1)),
                length(which(pred[,rfSSLName] == 1 & dataset[,binTarget] == 2)),
                length(which(pred[,rfSSLName] == 2 & dataset[,binTarget] == 1)),
                length(which(pred[,rfSSLName] == 2 & dataset[,binTarget] == 2)),
                length(which(pred[,rfSSLName] != dataset[,binTarget])) / 
                length(pred[,rfSSLName])))))
            predErrors <- rbind(predErrors, c(svmSSLName, as.character(c(
                length(which(pred[,svmSSLName] == 1 & dataset[,binTarget] ==1)),
                length(which(pred[,svmSSLName] == 1 & dataset[,binTarget] ==2)),
                length(which(pred[,svmSSLName] == 2 & dataset[,binTarget] ==1)),
                length(which(pred[,svmSSLName] == 2 & dataset[,binTarget] ==2)),
                length(which(pred[,svmSSLName] != dataset[,binTarget])) / 
                length(pred[,svmSSLName])))))
            predErrors <- rbind(predErrors, c(xgbSSLName, as.character(c( 
                length(which(pred[,xgbSSLName] == 1 & dataset[,binTarget] ==1)),
                length(which(pred[,xgbSSLName] == 1 & dataset[,binTarget] ==2)),
                length(which(pred[,xgbSSLName] == 2 & dataset[,binTarget] ==1)),
                length(which(pred[,xgbSSLName] == 2 & dataset[,binTarget] ==2)),
                length(which(pred[,xgbSSLName] != dataset[,binTarget])) / 
                length(pred[,xgbSSLName])))))

          } # end loop k clusters
        } # end if ssl
      } # end if varImg = NULL
    } # end loop varImg
  } # end loop varMain
} # end loop targets

# Save predictions to file
pred <- pred[,2:ncol(pred)]
predErrors <- data.frame(predErrors)
colnames(predErrors) <- predErrorNames
predErrors <- cbind(predErrors, rank = rank(predErrors$error))
write.csv(pred, file=paste0(outputFile, ".csv"))
write.csv(predErrors, file=paste0(outputFile, "_errors.csv"))
cat("\nResults saved in", outputFile, "\n\n")
