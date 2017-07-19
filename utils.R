library(randomForest, quietly=TRUE)

# Creates RF model of data and runs prediction on test set
# PARAMETERS:
# 	data = data matrix of all data
# 	target = string with col name of target variable in data
# 	input = Vector of col names of input variables
#	train = index of training observations
# 	test = index of testing observations
# 	seed = seed for RF, default is 42
# 	quietly = if true function will not print error matrix, default false
# 	trees = number of trees in forest, default 500
# OUTPUT: list containing:
#	model = rf model
#	train = index of observations used for training
#	test = index of observations used for testing 
#	prediction = prediction results on 
#	errorMatrix = error matrix
#	error = overall percent error

rfModel <- function(data,target,input,train,test,seed=42,quietly=FALSE,trees=500) {
	set.seed(seed)

	# Build random forest
	rf <- randomForest::randomForest(data[train,target]~.,
		data=data[train,c(input,target)],
		ntree=trees,
		mtrys=round(sqrt(ncol(data[,c(input,target)]))),
		importance=TRUE,
		na.action=randomForest::na.roughfix,
		replace=FALSE)

	if(!quietly) {
#		print(rf)
	}

	# Use RF model to predict test set and calculate error/matrix
	prediction <- predict(rf, data[test,c(input,target)], type="response")
	errorMatrix <- round(pcme(data[test,target], prediction), 2)
	if(!quietly) {
		print(errorMatrix)
		cat("Random Forest ( seed =", seed, 
			")  created with", percentError(errorMatrix), "% error\n")
		}

	return(list("model" = rf,
		"train" = train,
		"test" = test, 
		"prediction" = prediction, 
		"errorMatrix" = errorMatrix,
		"error" = percentError(errorMatrix)))
}

# PCME creates error matrix

pcme <- function(actual, cl) {
	x <- table(actual, cl)
	nc <- nrow(x)	# number of classes
	nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values
	tbl <- cbind(x/nv, Error=sapply(1:nc, function(r)
		round(sum(x[r, -r])/sum(x[r,]), 2)))
	names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
	return(tbl)	
}

# Gets error from error matrix
percentError <- function(errorMatrix) {
	return(100*round(1-sum(diag(errorMatrix), na.rm=TRUE), 2))
}

# Samples data and returns random train and test sets
# Checks to make sure target variables of sets are not missing any factors
# Input:
#	data: data matrix
#	target: name of target variable
#	split: percent of data in train set
# Returns: list containing:
#	train: index of training observations
#	test: index of testing observations
sampleSets <- function(data,target,split,checkTest=FALSE) {	
	missingFactors <- TRUE
	while(missingFactors) {
		train <- sort(sample(nrow(data), round(split*nrow(data))))
		test <- setdiff(seq_len(nrow(data)), train)

		if(checkTest) {
			if(length(levels(factor(data[train,target])))==
				length(levels(factor(data[,target]))) &&
				length(levels(factor(data[test,target])))==
				length(levels(factor(data[,target])))) {
				missingFactors <- FALSE 
			}
		} else {
			if(length(levels(factor(data[train,target])))==
				length(levels(factor(data[,target])))) {
				missingFactors <- FALSE
			}
		}
	}
	
	return(list("train" = train, "test" = test))
}

# Ranks input variables by highest correlation to target variable
# Returns vector of index of input in decreasing correlation
rankCor <- function(data, target, input) {
	cors <- vector()
	for(i in 1:length(input)) {
		cors <- c(cors, cor(data[,target],data[,input[i]]))	
	}
	cors <-abs(cors)
	cors <- cbind(cors,seq(1,length(cors)))
	cors <- cors[order(-cors[,1]),]
	return(cors[,2])
}
