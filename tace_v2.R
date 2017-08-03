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

# Set variable columns and convert to strings to numeric 
# (b/c XGB can only use numeric)
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

# stepwise regression subset selection
dataPreProcess <-preProcess(dataset[-17,c(ttpTarget,imgData)], 
	method = c("knnImpute"))
dataProcess <- predict(dataPreProcess, dataset[-17,c(ttpTarget,imgData)])
temp <- cor(dataProcess)
highCor <- findCorrelation(temp, cutoff = 0.9)
imgData <- setdiff(imgData, colnames(dataProcess)[highCor])
dataProcess2 <- dataProcess[,-highCor]
lm <- lm(dataProcess2[,ttpTarget]~.,data=dataProcess2[,imgData], 
	singular.ok=TRUE)
stpReg <- stepAIC(lm,direction="forward",trace=FALSE)
coefs <- summary(stpReg)$coefficients[,4]
varImg[[2]] <- names(coefs)[which(coefs < 0.15)]

# exhaustive regression selection
reg <- regsubsets(x=dataProcess[,imgData],y=dataProcess[,ttpTarget],
	really.big=T,nvmax=8)
fits <- coef(reg,8)
varImg[[3]] <- names(fits)[-1]
print(varImg[[3]])

# Create data frame to store predictions
pred <- data.frame(obs = seq(1,nrow(dataset),1))

for(i in 1:length(varMain)) {
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

pred <- pred[,2:ncol(pred)]
write.csv(pred, file="modelPredictions.csv")
cat("\nResults saved in modelPredictions.csv\n\n")
