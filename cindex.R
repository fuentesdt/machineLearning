library(compareC, quietly=TRUE)

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")
models <- read.csv("file:///home/gpauloski/git-repos/TACE/modelPredictions.csv")

#cat("\nBCLC:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_BCLC"]))
#cat("\nCLIP:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_CLIP"]))
#cat("\nTNM:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_TNM"]))
#cat("\nOkuda:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_Okuda"]),"\n")

output <- data.frame(model=as.character(seq(1,ncol(models[,-1]),1)))
output <- cbind(output, c_index=seq(1,ncol(models[,-1]),1))
for(i in 1:ncol(models[,-1])) {
	obs <- sort(c(which(models[,i+1]==1),which(models[,i+1]==2)))
	output$c_index[i] <- estC(dataset[obs,"liver_TTP"], 
		dataset[obs,"liver_CensorStatus"],models[obs,i+1])
	output$model[i] <- colnames(models)[i+1]
}

output$model <- colnames(models)[2:ncol(models)]
write.csv(output, file="model_c_index.csv")
cat("\nC-Index values saved to model_c_index.csv\n\n")
