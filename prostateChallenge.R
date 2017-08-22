library(randomForest, quietly=TRUE)
library(e1071, quietly=TRUE)
library(leaps, quietly=TRUE)
library(neuralnet, quietly=TRUE)
library(irr, quietly=TRUE)
source("drawTrees.R")
source("utils.R")
source("plotTree.R")

# Load data
data <- read.csv(paste("file:///home/gpauloski/git-repos/",
	"ProstateChallenge/truthadcdatamatrix.csv", sep=""), 
	na.strings=c(".", "NA", "", "?"))

# Model Parameters
partition <- 0.5	# % of data to be used in test set
split <- 4		# if 4; Z = 1 for ggg = 1,2,3
			# elseif 3; Z = 1 for ggg = 1,2
iterations <- 500		# number of iterations

# Create Z vector based on value of ggg
Z <- as.factor(ifelse(data$ggg < split, 1, 2))
data <- cbind(data, Z)
data$ggg <- as.factor(data$ggg)

# Define input/target/ignore variables
input <- c("Volume", "KTRANS.reslice", "T2Axial.norm", "ADC.reslice", 
	"T2Sag.norm", "T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4", 
	"BVAL.reslice")
target <- "Z"
input2 <- c(input, "Z2")
target2 <- "ggg"

# Create plots of correlation between variables in matrix
if(FALSE) {
	plotData <- data[sort(c(which(data[,target2]==2),
		which(data[,target2]==3))),c(target2,input)]
	plotData <- data[sample(seq_len(nrow(data)),),c(target2,input)]
	# Create vector of colors based on GGG value for plot
	pointColor <- vector()
	for(i in 1:nrow(plotData)) {
		if(plotData[i,target2]==1) {
			pointColor <- c(pointColor, "red")
		} else if(plotData[i,target2]==2) {
			pointColor <- c(pointColor, "green")
		} else if(plotData[i,target2]==3) {
			pointColor <- c(pointColor, "purple3")
		} else if(plotData[i,target2]==4) {
			pointColor <- c(pointColor, "blue")
		} else {
			pointColor <- c(pointColor, "black")
		} 
	}
	# Create plot. Saves to pdf file
	pairs(plotData[,2:ncol(plotData)],col=pointColor)
}

# Create column in dataset of fuzzy cmeans cluster
#fcm <- cmeans(data[,input],5)
#data <- cbind(data,fcm$cluster)
#colnames(data)[ncol(data)] <- "cluster"
#input <- c(input,"cluster")

# Create RF model(s)
# Create vector of random seeds
s <- round(10000*runif(iterations))
# Init empty vectors to save error pf each RF
errors1 <- vector()
errors2 <- vector()

input <- input[c(4,5,6)]

data <- data[sort(c(which(as.numeric(data[,target2])==1),
	which(as.numeric(data[,target2])==2))),]
data$ggg <- factor(data$ggg)

#plotTree(dataset=data,target=target2,input=input,
#    filename="Prostate_treeScatterMatrix")

#data$ggg <- droplevels(data$ggg)

for(i in 1:iterations) {

	cat("\nRUN :", i, "\n")
	dataset <- data
	doubleRF <- F

	if(doubleRF) {
	# Randomly sample two sets; one for each RF model
	sets <- sampleSets(data,target,0.5,checkTest=TRUE)
	set1 <- sets$train
	set2 <- sets$test
	
	# Sample test and train set using set1 for first RF model
	model1 <- sampleSets(dataset[set1,],target,0.7)
	modeltrain1 <- model1$train
	modeltest1 <- model1$test

	# Build RF Model with Z target
	result1 <- rfModel(dataset[set1,],target,input,modeltrain1,
		modeltest1,s[i])
	errors1 <- c(errors1, result1$error)	

	# Apply RF Model on set2 to predict Z
	Z2 <- as.numeric(predict(result1$model, dataset[,input], 
		type="response"))
	
	# Create new dataset with test data and predicted Z
	dataset <- cbind(dataset, Z2)
	
	# Sample test and train set usinf set2 for second RF model
	model2 <- sampleSets(dataset[set2,],target2,0.7)
	modeltrain2 <- model2$train
	modeltest2 <- model2$test
	
	# RFCV() gives error as function of num variables
	#print(rfcv(dataset[modeltrain2,input],dataset[modeltrain2,target2],
	#	step=1.5)$error.cv)

	# Build and Apply RF Model on new dataset to predict ggg	
	result2 <- rfModel(dataset[set2,],target2,input2,modeltrain2,
		modeltest2,s[i])
	#drawTrees(results2$model)
	errors2 <- c(errors2, result2$error)

	} else {
		sets <- sampleSets(dataset,target2,0.7)

		## Neural Net
#		dataset$ggg <- as.numeric(dataset$ggg)
#		nn <- neuralnet(ggg ~ Volume+KTRANS.reslice+
#			T2Axial.norm+ADC.reslice+T2Sag.norm+T2Axial.Entropy_4+
#			T2Axial.HaralickCorrelation_4+BVAL.reslice, 
#			data=dataset,hidden=100,stepmax=1000000,rep=10)
#		for(i in 1:10) {	
#			nnpred <- compute(nn, dataset[,input], rep=i)
#			print(round(nnpred$net.result[,1]))
#			print(dataset$ggg)
#			print(length(which(dataset$ggg!=round(
#				nnpred$net.result[,1])))/length(dataset$ggg))
#		}

		## Random Forest
		error3 <- NULL
		for(k in 1:50) {
		sets <- sampleSets(dataset,target2,(k+49)/100)
		rfm <- rfModel(dataset,target2,input,sets$train,sets$test,s[i],trees=1000)
		error3 <- c(error3, rfm$error)
		}
		errors1 <- cbind(errors1, error3)
		#drawTrees(rfm$model)
		#errors2 <- c(errors2, (length(which(dataset[sets$train,target2]
		#	!= predict(rfm$model, dataset[sets$train,input]))) / 
		#	length(sets$train)) * 100)

		## Support Vector Machine
#		svm <- svm(x=as.matrix(dataset[sets$train,input]),
#			y=dataset[sets$train,target])
			#kernal="polynomial", degree=3)
#		pred <- predict(svm,dataset[sets$test,input])
#		errors1 <- c(errors1, ((length(pred)-length(which(
#			dataset[sets$test,target]==pred)))/length(pred))*100)

		errors2 <- c(errors2,kappa2(cbind(dataset[sets$test,target2],
			rfm$prediction),weight="squared")$value)
	}
}

# Print summary of errors for each model
summary(errors1)
print(sd(errors1))
summary(errors2)
print(sd(errors2))
err <- cbind(seq(50,99,1),rowMeans(errors1),apply(errors1,1,sd))
write.csv(err, file="errorpercent.csv")
