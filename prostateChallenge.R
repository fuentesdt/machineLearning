source("utils.R")

# Load data

data <- read.csv("file:///home/gpauloski/git-repos/ProstateChallenge/truthadcdatamatrix.csv", 
	na.strings=c(".", "NA", "", "?"))

# Model Parameters

partition <- 0.7	# % of data to be used in test set
split <- 4		# if 4; Z = 1 for ggg = 1,2,3
			# elseif 3; Z = 1 for ggg = 1,2
iterations <- 1000	# number of iterations

# Create Z vector based on value of ggg

Z <- as.factor(ifelse(data$ggg < split, 1, 2))
data <- cbind(data, Z)
data$ggg <- as.factor(data$ggg)

# Define input/target/ignore variables

input <- c("KTRANS.reslice", "T2Axial.norm", "ADC.reslice", "T2Sag.norm", 
	"T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4", "BVAL.reslice")
target <- "Z"
input2 <- c(input, "Z2")
target2 <- "ggg"

# Create RF model(s)

s <- round(10000*runif(iterations))
errors1 <- vector()
errors2 <- vector()
for(i in 1:iterations) {

	cat("\nRUN :", i, "\n")
	dataset <- data

	sets <- sampleSets(data,target,0.5,checkTest=TRUE)
	set1 <- sets$train
	set2 <- sets$test

	# Build RF Model

	result1 <- rfModel(dataset[set1,],target,input,partition,s[i])
	errors1 <- c(errors1, result1$error)	

	# Apply RF Model on testing data to predict Z

	Z2 <- as.numeric(predict(result1$model, dataset[,c(input,target)], type="response"))
	
	# Create new dataset with test data and predicted Z

	dataset <- cbind(dataset, Z2)
	
	# Build and Apply RF Model on new dataset to predict ggg	
	result2 <- rfModel(dataset[set2,],target2,input2,partition,s[i])
	errors2 <- c(errors2, result2$error)

}

summary(errors1)
print(sd(errors1))
summary(errors2)
print(sd(errors2))
