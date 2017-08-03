library(randomForest, quietly=TRUE)	# RF
library(e1071, quietly=TRUE)		# SVM
library(xgboost, quietly=TRUE)		# XGboost
library(leaps, quietly=TRUE)		# Reg subset selection
library(MASS, quietly=TRUE)		# Stepwise reg subset selection
library(caret, quietly=TRUE)		# Caret for findCorrelation

# Load Data
dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")

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

## STEPWISE Selection ##
# Pre process data using knnimpute from caret package
dataPreProcess <-preProcess(dataset[-17,c(ttpTarget,imgData)], 
	method = c("knnImpute"))
# Impute data using preProcess to get new dataProcess containing no missing vals
dataProcess <- predict(dataPreProcess, dataset[-17,c(ttpTarget,imgData)])
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

## EXHAUSTIVE Selection ##
# run regsubsets to get best subset of 8 vars (Only 8 b/c it takes forever)
reg <- regsubsets(x=dataProcess[,imgData],y=dataProcess[,ttpTarget],
	really.big=T,nvmax=8)
# Get names of vars in best subset
fits <- coef(reg,8)
# Add best subset to exhaustive object in varImg
varImg[[3]] <- names(fits)[-1]
print(varImg[[3]])

# Create data frame to store predictions
pred <- data.frame(obs = seq(1,nrow(dataset),1))

# Loop for each of 4 baseline vars
for(i in 1:length(varMain)) {
	# Loop for each set of imgData
	for(j in 3:length(varImg)) {
		# Only build models if list of img data !null
		if(!(is.null(varImg[[j]]))) {
		cat("Building models with ", varMain[i], 
			names(varImg)[j], "\n")
	
		# Create input list using main vars and img vars
		input <- c(varMain[i], varImg[[j]])

		# Add new columns to pred data fram to store results
		pred <- cbind(pred, a = vector(length=nrow(dataset)),
			b = vector(length=nrow(dataset)),
			c = vector(length=nrow(dataset)))
		rfName <- paste0(varMain[i], "_", names(varImg)[j], "_", "rf")
		svmName <- paste0(varMain[i], "_", names(varImg)[j], "_", "svm")
		xgbName <- paste0(varMain[i], "_", names(varImg)[j], "_", "xgb")
		colnames(pred)[colnames(pred)=="a"] <- rfName
		colnames(pred)[colnames(pred)=="b"] <- svmName
		colnames(pred)[colnames(pred)=="c"] <- xgbName

		for(k in 1:nrow(dataset)) {
			#cat(k,"")
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
					y=dataset[train,binTarget])
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

# Save predictins to file
pred <- pred[,2:ncol(pred)]
write.csv(pred, file="modelPredictions.csv")
cat("\nResults saved in modelPredictions.csv\n\n")
