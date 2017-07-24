library(randomForest, quietly=TRUE)
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
input <- input[cors[1:200]]

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

#cors <- rankCor(dataset,initTarget,input)
#input <- input[sort(cors[1:10])]

print(input)

#iccor <- icc(dataset[,input])
#print(iccor$value)

#plotData <- dataset[sample(seq_len(nrow(dataset)),50),c(initTarget,input[7:12])]
plotData <- dataset[sample(seq_len(nrow(dataset)),50),c(initTarget,input[runif(6,1,length(input))])]
pointColor <- ifelse(plotData[,initTarget] > 21, "red", "blue")
pairs(plotData[,2:ncol(plotData)], col=pointColor)

break
###################

# Run rf

print("Building rf model(s)")
errors <- vector()
s <- round(10000*runif(iterations))

#sets <- sampleSets(dataset,target,partition)

for(i in 1:iterations) {
	cat("\nRUN:", i, "\n")
	sets <- sampleSets(dataset,target,partition)
	print(rfcv(dataset[sets$train,input],dataset[sets$train,target],scale="log",step=0.5)$error.cv)
	result<- rfModel(dataset,target,input,sets$train,sets$test,seed=s[i])
	errors <- c(errors, result$error)

}

summary(errors)
cat("\nSTD DEV of errors =", sd(errors),"\n")

