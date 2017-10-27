

datamatrixModeling_binary <- function(csvPath,target,inputs,
                                      rescale          = T,
                                      removeCorrelated = T,
                                      semisupervised   = T, 
                                      kClusters        = 9,
                                      genetic          = F,
                                      boruta           = T,
                                      univariate       = F,
                                      makeplots        = T
                                      ){ 

# Version: BINARY CLASSIFIER
# target and inputs are column headings in csv file,
# everything else is ignored
#
# Current version: plots not saved,
# use makeplots = F to disable plotting

cat(paste("\nTarget Variable: ",target, "\n"))

#packages
cat("Loading packages...\n\n")
libs <- c("caret", "e1071", "magrittr", "rpart", "nnet", "parallel", "randomForest", "xgboost", "Boruta", "leaps", "MASS", "ranger", "cluster", "subselect", "corrplot", "gridExtra")
lapply(libs, require,character.only=T)

#Load data
datamatrix <- read.csv(csvPath)

# Set model parameters
modelparams <- list(tree = list(method  = "rpart",
                                 #tuneGrid = data.frame(.cp = 0.01),
                                 parms   = list(split="information"),
                                 control = rpart.control(minsplit=20, minbucket=7,
                                                         usesurrogate=0, 
                                                         maxsurrogate=0)),
                     #method = "anova" removed since "method" is used in caret, 
                                          
                     forest = list(method      = "rf",
                                   ntree       = 500,
                                   #tuneGrid    = data.frame(.mtry = mtry), #default n/3 for regression!
                                   #replace    = TRUE,
                                   #na.action  = randomForest::na.roughfix,
                                   importance  = FALSE,
                                   predict.all = FALSE),
                     #na.action removed since "na.action" is used in caret
                    
                     xgboost = list(method = "xgbLinear"),
 
                     nnet = list(method = "nnet",
                                 #tuneGrid=data.frame(.size = 10, .decay = 0),
                                 #linout  = TRUE,
                                 skip    = TRUE,
                                 MaxNWts = 10000,
                                 trace   = FALSE,
                                 maxit   = 100),
                     
                     svm = list(method = "svmRadial")

)

# Trim dataset for convenience
dsraw <- datamatrix[ c(target, inputs) ]

# Pre-process data
#  methods: zv removes zero-variance columns
#           corr removes highly correlated columns
#           center/scale recenters variables
#           

ppMethods <- "zv"
if(rescale)          ppMethods <- c(ppMethods,"center","scale")
if(removeCorrelated) ppMethods <- c(ppMethods, "corr")

pp <-preProcess(dsraw[inputs], method = ppMethods, cutoff=0.9)
print(pp)

dsinputs <- predict(pp, newdata <- dsraw[inputs])

#get reduced input set/dataset
ds <- cbind(dsinputs,dsraw[target])
inputs <- setdiff(names(ds), target)


# Clusterin for semi-supervised analysis with kClusters
if(semisupervised){
cat(paste("Clustering into ", kClusters, " clusters... By unsupervised random forest\n\n"))
rfUL <- randomForest(x=ds[,inputs],
                     ntree = 500,
                     replace=FALSE)
ds <- cbind(ds, clusters.conv = pam(1-rfUL$proximity, k = kClusters, diss = TRUE, cluster.only = TRUE))
ds$clusters.conv <- as.factor(ds$clusters.conv) #change to faactor not int
inputs <- setdiff(names(ds),target)
}


# Variable selection 
variableSelections <- list(all=inputs)

#genetic
if(genetic){
gen <- genetic(cor(ds[,inputs]), 4) #manually selected 4 outputs
variableSelections$genetic <- names(ds[,inputs])[gen$bestsets]
print("Finished genetic variable selection")
}

#boruta
if(boruta){
bor <- Boruta(x=ds[,inputs], y=ds[,target]) #default values
variableSelections$boruta <- names(ds[,inputs])[which(bor$finalDecision == "Confirmed")]
print("Finished Boruta variable selection")
}

#univariate
if(univariate){

nums <- sapply(ds[inputs],is.numeric)
nums <- names(nums)[nums] #get names not T/F
pvals <- lapply(nums,
       function(var) {          
           formula    <- as.formula(paste(var, "~", target))
           test <- wilcox.test(formula, ds[, c(var, target)])
           test$p.value #could use 1-pchisq(test$statistic, df= test$parameter)
       })
variableSelections$univariate <- setdiff(inputs, nums[pvals > 0.05/length(nums)]) #discard variables above threshold

print("Finished univariate selection")
}



#WIP: use metric=roc for binary classification

#Modeling, dataparams are arguements to caret::train
modelformula <- as.formula(paste(target,"~."))
dataparams   <- list(form = modelformula,
#                      data = ds[,c(target,inputs)],
                      metric="Accuracy", #other option: AUC
                      trControl=trainControl(allowParallel  = T,
                                             method = "repeatedcv",
                                             number = 10,
                                             repeats= 5,
                                             verboseIter = F) # use method="none" to disable grid tuning for speed
                     )  
caretparams      <- lapply(modelparams,function(x) c(dataparams,x))

modelList <- list()
models    <- list()
for(jjj in 1:length(variableSelections)){
modeldata  <- ds[,c(target,variableSelections[[jjj]])]


print(paste("Training models using  ", names(variableSelections)[jjj], " variables"))

# seeding: Hawthorn et al, "The design and analysis of benchmark experiments" (2005)
for (iii in 1:length(modelparams)){
model_name <-paste(names(variableSelections)[jjj], names(modelparams)[iii],sep="_")
print(paste("Training model", iii, "of", (length(modelparams)*length(variableSelections)), ":", model_name, sep=" "))
set.seed(3141) #seed before train to get same subsamples
models[[model_name]] <- do.call(caret::train, c(caretparams[[iii]], list(data=modeldata)))
}

modelList[[names(variableSelections)[jjj]]] = models
}
cat("...Done training models\n\n")

# Get model accuracies
#WIP: why is accuracy only quoted to 2 decimals in summary?
rs <- resamples(models)
summary(object = rs)

#get best accuracy
acc <- rs$values[,grepl(rs$metrics[1], names(rs$values))]
names(acc) <- rs$models

maxacc <- max(apply(acc,2,mean))
maxmodels <- rs$models[apply(acc,2,mean)==maxacc]

cat(paste("best model(s): ", maxmodels, "\n"))
cat(sprintf("%s: %.4f \n", rs$metrics[1], maxacc))
#print(paste(rs$metrics[1], maxacc, sep=" : "))

#WIP save models/variable selections

if(makeplots){
#boxplot metrics over CV
# +1 to col arg keeps one box from being black
dev.new()
boxplot(acc,col=(as.numeric(as.factor(rs$methods))+1), las=2,
        ylab=paste("cross-validation",rs$metrics[1]))
legend("bottomright", legend=unique(rs$methods),
       fill=(as.numeric(as.factor(rs$methods))+1) )


# correlation plot if < 30 variables
  dev.new()
  par(mfrow=c(length(variableSelections),1))
  for(jjj in 1:length(variableSelections)){
   subsetName <- names( variableSelections)[[jjj]]  
   correlations <- cor(Filter(is.numeric,ds[variableSelections[[jjj]]]), use="pairwise")
   corrord <- order(correlations[1,])
   correlations <- correlations[corrord,corrord]

   corrplot(correlations,
   title = paste("Correlations for",subsetName)  )
  } #end for corrplots 

} #end if makeplots

# WIP: ROC curve for binary
# for best model call pROC::roc(rf$y, as.numeric(rf$predicted))








}
