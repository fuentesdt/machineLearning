library(randomForest, quietly=TRUE)
library(e1071, quietly=TRUE)
library(leaps, quietly=TRUE)
library(irr, quietly=TRUE)
library(cluster, quietly=TRUE)
source("drawTrees.R")
source("utils.R")
source("plotTree.R")

# Load data
dataset <- read.csv(paste("file:///home/gpauloski/git-repos/",
    "ProstateChallenge/truthadcdatamatrix.csv", sep=""), 
    na.strings=c(".", "NA", "", "?"))

# Model Parameters
partition <- 0.5    # % of data to be used in test set
split <- 4          # Low grade < split 
iterations <- 100     # number of iterations

# Create Z vector based on value of ggg
Z <- as.factor(ifelse(dataset$ggg < split, 1, 2))
dataset <- cbind(dataset, Z)
dataset$ggg <- as.factor(dataset$ggg)

# Define input/target/ignore variables
input <- c("Volume", "KTRANS.reslice", "T2Axial.norm", "ADC.reslice", 
    "T2Sag.norm", "T2Axial.Entropy_4", "T2Axial.HaralickCorrelation_4", 
    "BVAL.reslice")
target <- "Z"
input2 <- c(input, "Z2")
target2 <- "ggg"
input3 <- c(input, "clusters")

# Create RF model(s)
# Create vector of random seeds
seeds <- round(10000*runif(iterations))
# Init empty vectors to save error pf each RF
errors1 <- vector()
errors2 <- vector()

#dataset <- dataset[sort(c(which(as.numeric(dataset[,target2])==1),
#    which(as.numeric(dataset[,target2])==2))),]
#dataset$ggg <- factor(dataset$ggg)

#plotTree(dataset=dataset,target=target2,input=input,
#    filename="Prostate_treeScatterMatrix")

for(i in 1:iterations) {
  cat("RUN: ", i, "/", iterations, "\n",  sep="")
  set.seed(seeds[i])
  ds <- dataset
  sets <- sampleSets(ds,target2,0.7)

  ## RF w/ pam Clustering
  rfUL <- randomForest(x=dataset[,input], ntree=1000, replace=FALSE, 
      mtry=length(input), samplesize=nrows(dataset))
  clusters <- pam(1-rfUL$proximity, k=2, diss=TRUE, cluster.only=TRUE)
#  clusters <- pam(1-rfUL$proximity, k=5, diss=TRUE)
#  clusters$clustering <- round(runif(nrow(ds), 1, 5))

#  mds <- MDSplot(rfUL, dataset$ggg, k=2, pch=16, 
#      palette=c("red", "skyblue", "orange", "green", "darkblue"))
#  plot(mds$points[,1], mds$points[,2], pch=c(0,1,2,15,16), 
#      col=c("red", "skyblue", "orange", "green", "darkblue")[
#      as.numeric(ds$ggg)])
#  legend("topright", legend=unique(clusters$clustering), 
#      pch=c(0,1,2,15,16),
#      title="PAM cluster")
#  legend("topleft", legend=unique(clusters$clustering), pch=16,
#      col=c("red", "skyblue", "orange", "green", "darkblue"), title="GGG")
#  clusters <- clusters$clustering
   

  ## Upclass Clustering
#  library(upclass)
#  clModel <- upclassifymodel(Xtrain=as.matrix(ds[sets$train,input]),
#      cltrain=ds[sets$train, target2], Xtest=as.matrix(ds[sets$test,input]))
#  str(clModel)
#  clusters <- cbind(c(sets$train,sets$test), 
#      c(clModel$train$cl, clModel$test$cl))
#  str(clusters)
#  clusters <- clusters[order(clusters[,1]), 2]

  ds <- cbind(ds, clusters)
 
  ## Random Forest with clusters as input
  rf <- randomForest(ds[sets$train,target2]~., 
      data=ds[sets$train, input3], ntree=5000, replace=FALSE, 
      mtry=length(input3), samplesize=length(sets$train))
  rfPred <- predict(rf, ds[sets$test, input3])

#  rf <- list()
#  for(i in 1:2) {
#    clusterSet <- ds[sets$train,]
#    clusterSet <- clusterSet[which(ds$clusters == i),]
#    clusterSet$ggg <- factor(clusterSet$ggg)
#    rf[[i]] <- randomForest(clusterSet[, target2]~., 
#        data=clusterSet[,input], ntree=5000, replace=FALSE,
#        mtry=length(input), na.action=na.roughfix)
#  }
#  rfPred <- NULL
#  for(i in 1:length(sets$test)) {
#    if(clusters[sets$test[i]] == 1) 
#      rfPred <- c(rfPred, predict(rf[[1]], ds[sets$test[i], input]))
#    if(clusters[sets$test[i]] == 2) 
#      rfPred <- c(rfPred, predict(rf[[2]], ds[sets$test[i], input]))
#    if(clusters[sets$test[i]] == 3) 
#      rfPred <- c(rfPred, predict(rf[[3]], ds[sets$test[i], input]))
#    if(clusters[sets$test[i]] == 4) 
#      rfPred <- c(rfPred, predict(rf[[4]], ds[sets$test[i], input]))
#    if(clusters[sets$test[i]] == 5) 
#      rfPred <- c(rfPred, predict(rf[[5]], ds[sets$test[i], input]))
#  }

  errors1 <- c(errors1, (length(which(rfPred != ds[sets$test, 
      target2])) / length(rfPred)) * 100)

# drawTrees(rfm$model)
# errors2 <- c(errors2, (length(which(dataset[sets$train,target2]
#    != predict(rfm$model, dataset[sets$train,input]))) / 
#    length(sets$train)) * 100)

  ## Support Vector Machine
# svm <- svm(x=as.matrix(dataset[sets$train,input]),
#     y=dataset[sets$train,target])
#     kernal="polynomial", degree=3)
# pred <- predict(svm,dataset[sets$test,input])
# errors1 <- c(errors1, ((length(pred)-length(which(
#     dataset[sets$test,target]==pred)))/length(pred))*100)

  errors2 <- c(errors2,kappa2(cbind(ds[sets$test,target2],
      rfPred),weight="squared")$value)

}

# Print summary of errors for each model
summary(errors1)
print(sd(errors1))
summary(errors2)
print(sd(errors2))

