# Load dependencies
libs <- c("randomForest", "e1071", "xgboost", "leaps", "MASS", "ranger", 
    "caret", "cluster", "subselect", "Boruta")
invisible(suppressMessages(lapply(libs, library, quietly = TRUE, 
    character.only = TRUE)))

# Parameters/Load Data
dataset <- read.csv(paste0("file:///home/gpauloski/git-repos/",
    "ProstateChallenge/truthadcdatamatrix.csv"))
allVars <- TRUE
stepwise <- FALSE 
exhaustive <- TRUE 
anneal <- TRUE 
genetic <- FALSE 
boruta <- FALSE
semisupervised <- TRUE    # Perform semisupervised learning 
kClusters <- 10           # Number of clusters in SSL (Cluster 1 is skipped)
outputFile <- "model_predictions.csv"  # file save predictions of each model

# Set target columns and convert binary target to factor
targets <- c("ggg")
dataset$ggg <- factor(dataset$ggg)

# Create list of img data subsets
varImg <- list("volumes" = NULL, "stepwise" = NULL, "exhaustive" = NULL,
    "anneal" = NULL, "genetic" = NULL, "boruta" = NULL)

# Get list of col names for image data and remove empty columns
imgData <- c("Volume", "KTRANS.reslice", "T2Axial.norm", "ADC.reslice",
    "T2Sag.norm", "T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4",
    "BVAL.reslice")

if(allVars) {
  varImg[[1]] <- imgData
  print("All variable subset selection finished")
}

## STEPWISE Selection ##
# Stepwise AIC fails if num vars > num obs. If you get error that AIC is
# infinity, decrease the correlation coefficient cutoff. This will increase
# the number of corr vars removed from the stepwise regression
if(stepwise) {
  imgDataClean <- setdiff(imgData, colnames(dataset[,imgData])[findCorrelation(
    cor(dataset[,imgData]), cutoff=0.75)])
  # build linear model of dataset
  lm <- lm(dataset[,targets[1]]~.,data=dataset[,imgDataClean], 
      singular.ok=TRUE)
  # Perform forward stepwise regression on linear model 
  stpReg <- stepAIC(lm,direction="both",trace=FALSE)
  # Get best subset from regression output
  coefs <- summary(stpReg)$coefficients[,4]  
  # Add best subset to stepwise object in varImg
  varImg[[2]] <- names(coefs)[which(coefs < 0.15)]
  print("Stepwise subset selection finished")
}

## EXHAUSTIVE Selection ##
if(exhaustive) {
  # run regsubsets to get best subset of 4 vars (8 b/c it takes a while)
  reg <- regsubsets(x=dataset[,imgData],y=dataset[,targets[1]],
      method="exhaustive",really.big=T,nvmax=4)
  # Get names of vars in best subset
  fits <- coef(reg,4)
  # Add best subset to exhaustive object in varImg
  varImg[[3]] <- names(fits)[-1]
  print("Exhaustive subset selection finished")
}

## ANNEAL Selection ##
if(anneal) {
  # Default to 8 variables selected in best model
  # Anneal can be highly tuned, may be wise to test it out more
  ann <- anneal(cor(dataset[,imgData]),4)
  varImg[[4]] <- names(dataset[,imgData])[ann$bestsets]
  print("Annealing subset selection finished")
}

## GENETIC Selection ##
if(genetic) {
  gen <- genetic(cor(dataset[,imgData]),4, maxclone=1)
  varImg[[5]] <- names(dataset[imgData])[gen$bestsets]
  print("Genetic subset selection finished")
}

## BORUTA Selection ##
if(boruta) {
  bor <- Boruta(x=dataset[,imgData], y=dataset[, targets[1]], pValue=0.5)
  varImg[[6]] <- names(dataset[,imgData])[
      which(bor$finalDecision == "Confirmed")]
  print("Boruta subset selection finished")
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

    # Loop for each set of imgData
    for(j in 1:length(varImg)) {

      # Only build models if list of img data !null
      if(!(is.null(varImg[[j]]))) {

        # Loop for each in iters. See iters variable for more info
        for(k in 1:iters) {
          cat("Building Models: target=", targets[h], ", input=",
              names(varImg)[j], sep="")
          # Create copy of dataset to work in; prevents overwrites
          ds <- dataset
          # Add new columns to pred data frame to store results
          pred <- cbind(pred, a = vector(length=nrow(ds)),
              b = vector(length=nrow(ds)), c = vector(length=nrow(ds)))
          # Create name of column using var inputs and ML algorithm
          rfName <- paste0(targets[h], "_", names(varImg)[j], "_", "rf")
          svmName <- paste0(targets[h], "_", names(varImg)[j], "_", "svm")
          xgbName <- paste0(targets[h], "_", names(varImg)[j], "_", "xgb")

          # If k = 1, perform supervised learning
          if(k == 1) {
            cat(", Supervised", "\n", sep="")
            # Build input var list by combining baseline and imgData vars
            input <- varImg[[j]]
            # Set colnames of data frame to these names
            colnames(pred)[colnames(pred)=="a"] <- rfName
            colnames(pred)[colnames(pred)=="b"] <- svmName
            colnames(pred)[colnames(pred)=="c"] <- xgbName

          # Else k != 1, perfrom semi-supervised learning
          } else {
            cat(", Semi-super (k=", k, ")\n", sep="")
            rfName <- paste0(rfName, "_k", k)
            svmName <- paste0(svmName, "_k", k)
            xgbName <- paste0(xgbName, "_k", k)
            # Set colnames of data frame to these names
            colnames(pred)[colnames(pred)=="a"] <- rfName
            colnames(pred)[colnames(pred)=="b"] <- svmName
            colnames(pred)[colnames(pred)=="c"] <- xgbName
            # Unsupervised random forest and add classifications to dataset
            rfUL <- randomForest(x=ds[,varImg[[j]]], 
                ntree=1000, replace=FALSE, mtry=length(varImg[[j]]), 
                na.action=na.roughfix)
            ds <- cbind(ds, clusters = pam(1-rfUL$proximity, k=k, diss=TRUE, 
                cluster.only = TRUE))
            # Build input var list by combining baseline and imgData vars
            input <- c(varImg[[j]], "clusters")
          }

          # For each obs in dataset, leave one out and train model on
          # remaining obs in dataset then use obs that was left out as test obs
          for(m in 1:nrow(ds)) {
            # Create vector of training obs missing one variable
            train <- seq(1,nrow(ds),1)
            train <- train[-m]

            ## RANDOM FOREST ##
            rf <- randomForest(ds[train,targets[h]]~.,
                data=ds[train,input], ntrees=10000, mtry=length(input),
                replace=FALSE, na.action=na.roughfix)
            pred[m,rfName] <- predict(rf, ds[m, input])

            ## SVM ##
            svm <- svm(x=as.matrix(ds[train,input]),
                y=ds[train,targets[h]], kernel="polynomial", degree=3)
            if(!(any(which(is.na(ds[m,input]))))) {
              pred[m,svmName] <- predict(svm, ds[m,input])
            } else pred[m,svmName] <- "NA"			

            ## XGBOOST ##
            trnList <- list("data" = as.matrix(ds[train,input]), 
                "label" = ifelse(ds[train,targets[h]] == 1, 0, 1))
            tstList <- list("data" = as.matrix(ds[m,input]), 
                "label" = ifelse(ds[m,targets[h]] == 1, 0, 1))
            bst <- xgboost(trnList$data, label=trnList$label, nrounds=2, 
                 objective="binary:logistic", verbose=0)
            pred[m,xgbName] <- ifelse(round(predict(bst, 
                tstList$data)) == 0, 1, 2)
          }

          # Calculate errors and error matrix
          predErrors <- rbind(predErrors, 
              c(rfName, errorMatrix(pred[,rfName], ds[,targets[h]])),
              c(svmName, errorMatrix(pred[,svmName], ds[,targets[h]])),
              c(xgbName, errorMatrix(pred[,xgbName], ds[,targets[h]])))

        } # end loop iters
      } # end if varImg = NULL
    } # end loop varImg
} # end loop targets

# Transpose pred matrix and remove index vector
cat("Model building finished. Saving results...\n")
pred <- t(pred[,2:ncol(pred)])
# Convert predErrors to data frame and give it column names
predErrors <- data.frame(predErrors)
colnames(predErrors) <- predErrorNames
# Combine predErrors and pred
predErrors <- cbind(predErrors, rank = rank(predErrors$error), pred)
# Clean up colnames and modelIDs
for(i in 8:ncol(predErrors)) colnames(predErrors)[i] <- paste0("obs_", i-7) 
predErrors[,1] <- gsub("_liver_", "_", predErrors[,1])
# Remove error matrix columns b/c that only worked for binary classifications
predErrors <- predErrors[,-(2:5)]
# Write predErrors to csv
write.csv(predErrors, file=  outputFile, row.names = FALSE)
cat("Results saved in", outputFile, "\n\n")
