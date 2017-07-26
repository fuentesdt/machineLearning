library(randomForest, quietly=TRUE)
library(e1071, quietly=TRUE)
source("drawTrees.R")
source("utils.R")

# Load data
data <- read.csv("file:///home/gpauloski/git-repos/ProstateChallenge/truthadcdatamatrix.csv", 
	na.strings=c(".", "NA", "", "?"))

# Model Parameters
partition <- 0.5	# % of data to be used in test set
split <- 4		# if 4; Z = 1 for ggg = 1,2,3
			# elseif 3; Z = 1 for ggg = 1,2
iterations <- 1		# number of iterations

# Create Z vector based on value of ggg
Z <- as.factor(ifelse(data$ggg < split, 1, 2))
data <- cbind(data, Z)
data$ggg <- as.factor(data$ggg)

# Define input/target/ignore variables
input <- c("Volume", "KTRANS.reslice", "T2Axial.norm", "ADC.reslice", "T2Sag.norm", 
	"T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4", "BVAL.reslice")
target <- "Z"
input2 <- c(input, "Z2")
target2 <- "ggg"

# Transform Data
#for (i in 1:1) {
#	data[,input[i]] <- log(data[,input[i]])
#}

# Create plots of correlation between variables in matrix
# Sample 30 observations for plots
#plotData <- data[sample(seq_len(nrow(data)),),c(target2,input)]
# Create vector of colors based on GGG value for plot
#pointColor <- vector()
#for(i in 1:nrow(plotData)) {
#	if(plotData[i,target2]==1) #{
#		pointColor <- c(pointColor, "red")
#	} else if(plotData[i,target2]==2) {
#		pointColor <- c(pointColor, "green")
#	} else if(plotData[i,target2]==3) {
#		pointColor <- c(pointColor, "purple3")
#	} else if(plotData[i,target2]==4) {
#		pointColor <- c(pointColor, "blue")
#	} else {
#		pointColor <- c(pointColor, "black")
#	} 
#}
# Create plot. Saves to pdf file
#pairs(plotData[,2:ncol(plotData)],col=pointColor)

# Create column in dataset of fuzzy cmeans cluster
#fcm <- cmeans(data[,input],5)
#data <- cbind(data,fcm$cluster)
#colnames(data)[ncol(data)] <- "cluster"
#input <- c(input,"cluster")

# Create PDF of diagram for each tree
sets <- sampleSets(data,target2,0.7)
rf <- rfModel(data,target2,input,sets$train,sets$test,42)
drawTrees(rf$model)

# Create RF model(s)
# Create vector of random seeds
s <- round(10000*runif(iterations))
# Init empty vectors to save error pf each RF
errors1 <- vector()
errors2 <- vector()
for(i in 1:iterations) {

	cat("\nRUN :", i, "\n")
	dataset <- data

	# Randomly sample two sets; one for each RF model
	sets <- sampleSets(data,target,0.5,checkTest=TRUE)
	set1 <- sets$train
	set2 <- sets$test
	
	# Sample test and train set using set1 for first RF model
	model1 <- sampleSets(dataset[set1,],target,0.7)
	modeltrain1 <- model1$train
	modeltest1 <- model1$test

	# Build RF Model with Z target
	result1 <- rfModel(dataset[set1,],target,input,modeltrain1,modeltest1,s[i])
	errors1 <- c(errors1, result1$error)	

	# Apply RF Model on set2 to predict Z
	Z2 <- as.numeric(predict(result1$model, dataset[,c(input,target)], type="response"))
	
	# Create new dataset with test data and predicted Z
	dataset2 <- cbind(dataset[set2,], Z2)
	
	# Sample test and train set usinf set2 for second RF model
	model2 <- sampleSets(dataset2,target2,0.7)
	modeltrain2 <- model2$train
	modeltest2 <- model2$test
	
	# RFCV() gives error as function of num variables
	#print(rfcv(dataset[modeltrain2,input],dataset[modeltrain2,target2],step=1.5)$error.cv)

	# Build and Apply RF Model on new dataset to predict ggg	
	result2 <- rfModel(dataset2,target2,input,modeltrain2,modeltest2,s[i])
	errors2 <- c(errors2, result2$error)

}

# Print summary of errors for each model
summary(errors1)
print(sd(errors1))
summary(errors2)
print(sd(errors2))
