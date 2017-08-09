library(randomForest, quietly=TRUE)	# RF
library(e1071, quietly=TRUE)		# SVM
library(xgboost, quietly=TRUE)		# XGboost
library(leaps, quietly=TRUE)		# Reg subset selection
library(MASS, quietly=TRUE)		# Stepwise reg subset selection
library(caret, quietly=TRUE)		# Caret for findCorrelation
source("utils.R")			# RankCor function used to rank vars

# Parameters/Load Data
dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")
stepwise <- TRUE	# If TRUE: perform stepwise model selection
exhaustive <- FALSE	# If TRUE: perform exhaustive model selection
outputFile <- "model_predictions.csv"

# Set target columns and convert binary target to factor
ttpTarget <- "liver_TTP"
binTarget <- "TTP1.NR.2.R"
dataset[,binTarget] <- as.factor(dataset[,binTarget])

# Create new target for xgboost b/c it can only use binary num vector of
# ones and zeros and add that to dataset
binNumTarget <- "xgbTTP"
xgbTTP <- ifelse(dataset[,binTarget]==1,0,1)
dataset <- cbind(xgbTTP,dataset)

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
varImg$volumes <- c("liver_Volume", "necrosis_Volume", 
		"viable_Volume", "viable_Art_DENOISE")

# Get list of col names for image data and remove empty columns
imgData <- NULL
imgDataIndex <- seq(which(colnames(dataset)==varImg$volumes[1]),ncol(dataset),1)
for(i in 1:length(imgDataIndex)) {
	if(length(which(is.na(dataset[,imgDataIndex[i]]))) < 10) {
		imgData <- c(imgData, colnames(dataset)[imgDataIndex[i]])
	}
}

if(stepwise) {
	## STEPWISE Selection ##
	# Pre process data using knnimpute from caret package
	dataPreProcess <-preProcess(dataset[-17,c(ttpTarget,imgData)], 
		method = c("knnImpute"))
	# Impute data using preProcess to get new dataProcess w/ no missing vals
	dataProcess <- predict(dataPreProcess,dataset[-17,c(ttpTarget,imgData)])
	# Get correlation coefs of dataset
	temp <- cor(dataProcess)
	# Find variables with corr coef > 0.9
	highCor <- findCorrelation(temp, cutoff = 0.9)
	# Remove highCor vars from imgData vector
	imgData <- setdiff(imgData, colnames(dataProcess)[highCor])
	# Remove highCor vars from dataset and create new dataset 
	dataProcess2 <- dataProcess[,-highCor]
	# build linear model of dataset
	lm <- lm(dataProcess2[,ttpTarget]~.,data=dataProcess2[,imgData], 
		singular.ok=TRUE)
	# Perform forward stepwise regression on linear model 
	stpReg <- stepAIC(lm,direction="forward",trace=FALSE)
	# Get best subset from regression output
	coefs <- summary(stpReg)$coefficients[,4]
	# Add best subset to stepwise object in varImg
	varImg[[2]] <- names(coefs)[which(coefs < 0.15)]
}

# Create scatter plots of data
#pdf(height=12,width=12)
#plotData <- rankCor(dataProcess2, ttpTarget, imgData)
#pointColor <- ifelse(dataset[,ttpTarget] > 21, "blue", "red")
#pairs(dataProcess[,c(ttpTarget,colnames(dataProcess2)[plotData[6:10]])], 
#	col=pointColor)
#pairs(dataProcess[,c(ttpTarget,varImg[[2]][6:10])], col=pointColor)

if(exhaustive) {
	## EXHAUSTIVE Selection ##
	# run regsubsets to get best subset of 8 vars (8 b/c it takes a while)
	reg <- regsubsets(x=dataProcess[,imgData],y=dataProcess[,ttpTarget],
		really.big=T,nvmax=8)
	# Get names of vars in best subset
	fits <- coef(reg,8)
	# Add best subset to exhaustive object in varImg
	varImg[[3]] <- names(fits)[-1]
	print(varImg[[3]])
}

# Create data frame to store predictions for each model
pred <- data.frame(obs = seq(1,nrow(dataset),1))

library(tree,quietly=TRUE)
library(maptree,quietly=TRUE)
dTree <- tree(TTP1.NR.2.R~.,data=dataset[,c(binTarget,imgData)])
pred <- predict(dTree, dataset[,imgData], type="class")
pdf(width=11,height=8.5)
draw.tree(dTree,cex=.5,nodeinfo=TRUE)
treeError <- (length(which(dataset[,binTarget]!=pred))/length(pred))*100
cat("\nSingle Decision tree error prediciting training set:", treeError, "\n")

break
# Loop for each of 4 baseline vars
for(i in 1:length(varMain)) {

	# Loop for each set of imgData
	for(j in 1:length(varImg)) {

		# Only build models if list of img data !null
		if(!(is.null(varImg[[j]]))) {
		cat("Building models with ", varMain[i], names(varImg)[j], "\n")
	
		# Build input var list by combining baseline and imgData vars
		input <- c(varMain[i], varImg[[j]])
		# Add volumes to stepwise
		#if(j==2) input <- c(input,varImg[[1]])

		# Add new columns to pred data frame to store results
		pred <- cbind(pred, a = vector(length=nrow(dataset)),
			b = vector(length=nrow(dataset)),
			c = vector(length=nrow(dataset)))
		# Create name of column using var inputs and ML algorithm
		rfName <- paste0(varMain[i], "_", names(varImg)[j], "_", "rf")
		svmName <- paste0(varMain[i], "_", names(varImg)[j], "_", "svm")
		xgbName <- paste0(varMain[i], "_", names(varImg)[j], "_", "xgb")
		# Set colnames of data frame to these names
		colnames(pred)[colnames(pred)=="a"] <- rfName
		colnames(pred)[colnames(pred)=="b"] <- svmName
		colnames(pred)[colnames(pred)=="c"] <- xgbName

		# For each obs in dataset, leave one out and train model on
		# remaining obs in dataset then use obs that was left out as
		# test obs
		for(k in 1:nrow(dataset)) {
			# Create vector of training obs missing one variable
			train <- seq(1,nrow(dataset),1)
			train <- train[-k]

			## RANDOM FOREST ##
			rf <- randomForest::randomForest(
				dataset[train,binTarget]~.,
				data=dataset[train,input],
				ntrees=500,
				mtrys=round(sqrt(ncol(dataset[,input]))),
				na.action=randomForest::na.roughfix)
			pred[k,rfName] <- predict(rf,dataset[k,input])

			## SVM ##
			svm <- svm(x=as.matrix(dataset[train,input]),
					y=dataset[train,binTarget],
					kernel="polynomial",
					degree=3)
			if(!(any(which(is.na(dataset[k,input]))))) {
				pred[k,svmName] <- predict(svm, 
					dataset[k,input])
			} else pred[k,svmName] <- "NA"			
	
			## XGBOOST ##
			trnList <- list("data" = as.matrix(
					dataset[train,input]), 
					"label" = dataset[train,binNumTarget])
			tstList <- list("data" = as.matrix(dataset[k,input]), 
				"label" = dataset[k,binNumTarget])
			bst <- xgboost(trnList$data, label=trnList$label, 
				nrounds=2, objective="binary:logistic",
				verbose=0)
			pred[k,xgbName] <- ifelse(round(predict(bst, 
				tstList$data))==0,1,2)
		}}
	}
}

# Save predictions to file
pred <- pred[,2:ncol(pred)]
write.csv(pred, file=outputFile)
cat("\nResults saved in", outputFile, "\n\n")
