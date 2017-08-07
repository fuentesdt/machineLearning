library(randomForest, quietly=TRUE)
library(leaps, quietly=TRUE)
source("utils.R")
source("drawTrees.R")

# Load Data

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/TACE%20data%20features%20cleaned.csv")

initTarget <- "liver_TTP"	# Target variable; will be refactored into binary categories
split <- 21			# Target data greater than this variable will be 1; else 0
partition <- 0.7 		# % of data in training set
iterations <- 10		# Number of rf iterations

# Create list of input variables
print("Creating input variable list")
input <- NULL
ignore <- c(initTarget, "liver_Metastasis..yes.1..No.0.", 
	"liver_Lymphnodes..yes.1..No.0.")
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
cors <- rankCor(dataset,initTarget,input)
# Extract top X variables by correlation
input <- input[cors[1:100]]

# Removes variables that are highly correlated to reduce redundancy in data
# Set loop=TRUE to do this. Correlation threshhold > 0.8
loop <- FALSE
while(loop) {
	cmb <- combn(length(input),2)
	cors2 <- vector()
	continue <- FALSE
	for(i in 1:ncol(cmb)) {
		cors2 <- c(cors2,abs(cor(dataset[,input[cmb[1,i]]],
			dataset[,input[cmb[2,i]]])))
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

#input <- c("liver_Del_SIGMA_RADIUS_1", "necrosis_Art_SKEWNESS_RADIUS_1",
#		"viable_Ven_SIGMA_RADIUS_1", "necrosis_Ven_SIGMA_RADIUS_1",
#		"necrosis_Art_ATROPOS_GMM_POSTERIORS2", 
#		"viable_Ven_SIGMA_RADIUS_5", "viable_Del_SIGMA_RADIUS_5", 
#		"liver_Ven_ATROPOS_GMM_POSTERIORS1")

#pdf(height=12,width=12)
#reg <- regsubsets(x=dataset[,input],y=dataset[,initTarget], really.big=T)
#plot(reg, scale="Cp", main="Cp")
#fits <- coef(reg, 20)
#input <- names(fits)[2:21]
#print(input)

# Plot data matrix to see correlations between variables. Saves to pdf
#plotData <- dataset[sample(seq_len(nrow(dataset)),50),c(initTarget,input[7:12])]
#plotData <- dataset[sample(seq_len(nrow(dataset)),50),c(initTarget,input[runif(6,1,length(input))])]
#pointColor <- ifelse(plotData[,initTarget] > 21, "red", "blue")
#pairs(plotData[,2:ncol(plotData)], col=pointColor)

# Create vector for errors and random seeds
print("Building rf model(s)")
errors <- vector()
s <- round(10000*runif(iterations))

# Use same sample set for each iteration
#sets <- sampleSets(dataset,target,partition)

# Build n randomForest w/ random seed where n=iterations
for(i in 1:iterations) {
	cat("\nRUN:", i, "\n")

	# Randomly sample test and train set
	sets <- sampleSets(dataset,target,partition)
	
	# Display error as function of num variables
	#print(rfcv(dataset[sets$train,input],dataset[sets$train,target],scale="log",step=0.5)$error.cv)

	# Build RF model and save error result
	rfm<- rfModel(dataset,target,input,sets$train,sets$test,seed=s[i])
	errors <- c(errors, rfm$error)
}

print(input)
drawTrees(rfm$model, filename="LiverTreeDiagrams_regSel.pdf")
summary(errors)
cat("\nSTD DEV of errors =", sd(errors),"\n")

