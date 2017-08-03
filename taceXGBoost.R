library(xgboost, quietly=TRUE)
library(irr, quietly=TRUE)
source("utils.R")

# Load Data

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")

initTarget <- "liver_TTP"	# Target variable; will be refactored into binary categories
split <- 21			# Target data greater than this variable will be 1; else 0
partition <- 0.7 		# % of data in training set
iterations <- 1

# Create list of input variables

print("Creating input variable list")
input <- NULL
inputTemp <- seq(45,ncol(dataset),1)
inputTemp <- c(11,14,24,inputTemp)
input <- NULL
for(i in 1:length(inputTemp)){
	if(!(any(is.na(dataset[-17,inputTemp[i]])))) input <- c(input,inputTemp[i])
}
target <- "TTP1.NR.2.R"
dataset[,target] <- as.factor(dataset[,target])
#ignore <- c(initTarget, "liver_Metastasis..yes.1..No.0.", "liver_Lymphnodes..yes.1..No.0.")
#if(is.null(input)) { 
#	coln <-  colnames(dataset)
#	input <- setdiff(coln, ignore)
#	}

# Create binary classification of target

#print("Converting target data to binary classification")
#binaryTarget <- as.factor(ifelse(dataset[,initTarget] > 21, 1, 0))
#dataset <- cbind(binaryTarget, dataset)
#target = "binaryTarget"

# Rank input variables by highest correlation
# Only use top 50 variable

#cors <- rankCor(dataset,initTarget,input)
#input <- input[sort(cors[1:10])]

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

#sets <- sampleSets(dataset, initTarget, partition)
#train <- list("data" = as.matrix(dataset[sets$train,input]), "label" = dataset[sets$train,initTarget])
#test <- list("data" = as.matrix(dataset[sets$test,input]), "label" = dataset[sets$test,initTarget])
#
#bst <- xgboost(train$data,
#		
#		label=train$label,
#		booster="gblinear",
#		objective="reg:linear")
#pred <- predict(bst, test$data)

dataset[,target] <- ifelse(dataset[,target]==1,0,1)

prd <- NULL
for(i in 1:nrow(dataset)) {
	if(i!=17){
		trn <- seq(1,nrow(dataset),1)
		trn <- trn[-i]
		train <- list("data" = as.matrix(dataset[trn,input]), "label" = dataset[trn,target])
		test <- list("data" = as.matrix(dataset[i,input]), "label" = dataset[i,target])
		bst <- xgboost(train$data, label=train$label, nrounds=2, objective="binary:logistic")
		prd <- c(prd,predict(bst, test$data))
	} else prd <- c(prd,8)
}

print(ifelse(round(prd)==0,1,2))
print(dataset[,target])


