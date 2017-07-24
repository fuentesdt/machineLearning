library(xgboost, quietly=TRUE)
library(irr, quietly=TRUE)
source("utils.R")

# Load Data

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/TACE%20data%20features%20cleaned.csv")

initTarget <- "liver_TTP"	# Target variable; will be refactored into binary categories
split <- 21			# Target data greater than this variable will be 1; else 0
partition <- 0.7 		# % of data in training set
iterations <- 1

# Create list of input variables

print("Creating input variable list")
input <- NULL
ignore <- c(initTarget, "liver_Metastasis..yes.1..No.0.", "liver_Lymphnodes..yes.1..No.0.")
if(is.null(input)) { 
	coln <-  colnames(dataset)
	input <- setdiff(coln, ignore)
	}

# Create binary classification of target

print("Converting target data to binary classification")
binaryTarget <- as.factor(ifelse(dataset[,initTarget] > 21, 1, 0))
dataset <- cbind(binaryTarget, dataset)
target = "binaryTarget"

# Rank input variables by highest correlation
# Only use top 50 variable

cors <- rankCor(dataset,initTarget,input)
input <- input[sort(cors[1:10])]

# FIX THIS
loop <- FALSE
while(loop) {
	cmb <- combn(length(input),2)
	cors2 <- vector()
	continue <- FALSE
	for(i in 1:ncol(cmb)) {
		cors2 <- c(cors2,abs(cor(dataset[,input[cmb[1,i]]],dataset[,input[cmb[2,i]]])))
		if(cors2[i] > 0.8) {
			input <- input[-cmb[1,i]]
			continue <- TRUE
			break
		}	
	}
	if(!continue) {
		loop <- FALSE
	}
}
#print(input)

#iccor <- icc(dataset[,input])
#print(iccor$value)

# Run rf

print("Building rf model(s)")

sets <- sampleSets(dataset, initTarget, partition)
train <- list("data" = as.matrix(dataset[sets$train,input]), "label" = dataset[sets$train,initTarget])
test <- list("data" = as.matrix(dataset[sets$test,input]), "label" = dataset[sets$test,initTarget])

bst <- xgboost(train$data,
		
		label=train$label,
		booster="gblinear",
		objective="reg:linear")
pred <- predict(bst, test$data)
