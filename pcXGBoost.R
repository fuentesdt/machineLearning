library(xgboost, quietly=TRUE)
source("utils.R")

# Load data

dataset <- read.csv("file:///home/gpauloski/git-repos/ProstateChallenge/truthadcdatamatrix.csv", 
	na.strings=c(".", "NA", "", "?"))

# Model Parameters

partition <- 0.7	# % of data to be used in test set
split <- 3 		# if 4; Z = 1 for ggg = 1,2,3
			# elseif 3; Z = 1 for ggg = 1,2
iterations <- 1000	# number of iterations

# Create Z vector based on value of ggg

Z <- as.numeric(ifelse(dataset$ggg < split, 0, 1))
dataset <- cbind(dataset, Z)
dataset$ggg <- as.numeric(dataset$ggg)
dataset$ggg <- dataset$ggg-1

# Define input/target/ignore variables

input <- c("KTRANS.reslice", "T2Axial.norm", "ADC.reslice", "T2Sag.norm", 
	"T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4", "BVAL.reslice")
target <- "Z"
input2 <- c(input, "Z2")
target2 <- "ggg"

# Create train/test set

sets <- sampleSets(dataset,target2,partition)
train <- list("data" = as.matrix(dataset[sets$train,input]), "label" = dataset[sets$train,target])
test <- list("data" = as.matrix(dataset[sets$test,input]), "label" = dataset[sets$test,target])

bst <- xgboost(train$data,
		label=train$label,
		nrounds=7,
		#num_class=5,
		#eval_metric="mlogloss",
		#nfold=5,
		objective="binary:logistic")
pred <- predict(bst, test$data)
#pred <- matrix(pred, nrow=5, ncol=length(pred)/5)
print(round(pred))
print(test$label)
print(length(which(round(pred)==test$label))/length(pred))
	
