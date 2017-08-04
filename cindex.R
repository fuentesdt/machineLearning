library(compareC, quietly=TRUE)

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")
models <- read.csv("file:///home/gpauloski/git-repos/TACE/modelPredictions.csv")

#cat("\nBCLC:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_BCLC"]))
#cat("\nCLIP:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_CLIP"]))
#cat("\nTNM:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_TNM"]))
#cat("\nOkuda:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_Okuda"]),"\n")

cindex <- function(time, pred) {
	nobs <- length(time)
	conc <- 0
	tie <- 0
	npairs <- 0

	for(i in 1:nobs) {
		for(j in 1:nobs) {
			if(i==j) break
			npairs = npairs + 1	
			if(time[i] > time[j]) {
				if(pred[i] == pred[j]) tie = tie + 1
				if(pred[i] > pred[j]) conc = conc + 1
			} else if(time[i] < time[j]) {
				if(pred[i] == pred[j]) tie = tie + 1
				if(pred[i] < pred[j]) conc = conc + 1
			} else tie = tie + 1
		}
	}

	return((conc+(0.5*tie))/npairs)
} 


output <- data.frame(model=as.character(seq(1,ncol(models[,-1]),1)))
output <- cbind(output, c_index=seq(1,ncol(models[,-1]),1))
output <- cbind(output, c_index2 = output$c_index)
for(i in 1:ncol(models[,-1])) {
	obs <- sort(c(which(models[,i+1]==1),which(models[,i+1]==2)))
	output$c_index[i] <- estC(dataset[obs,"liver_TTP"], 
		dataset[obs,"liver_CensorStatus"],models[obs,i+1])
	output$model[i] <- colnames(models)[i+1]
	output$c_index2[i] <- cindex(dataset[obs,"liver_TTP"],models[obs,i+1])
}

output$model <- colnames(models)[2:ncol(models)]
write.csv(output, file="model_c_index.csv")
cat("\nC-Index values saved to model_c_index.csv\n\n")



