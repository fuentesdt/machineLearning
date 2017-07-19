source("utils.R")

# Load Data

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/TACE%20data%20features%20cleaned.csv")

initTarget <- "liver_TTP"	# Target variable; will be refactored into binary categories
split <- 21			# Target data greater than this variable will be 1; else 0
partition <- 0.7 		# % of data in training set
iterations <- 50

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
input <- input[sort(cors[1:50])]

# Run rf

print("Building rf model(s)")
errors <- vector()
s <- round(10000*runif(iterations))

#sets <- sampleSets(dataset,target,partition)

for(i in 1:iterations) {
	cat("\nRUN:", i, "\n")
	sets <- sampleSets(dataset,target,partition)
	result<- rfModel(dataset,target,input,sets$train,sets$test,seed=s[i])
	errors <- c(errors, result$error)

}

summary(errors)
cat("\nSTD DEV of errors =", sd(errors),"\n")

