library(compareC, quietly=TRUE)

dataset <- read.csv("file:///home/gpauloski/git-repos/TACE/gmmdatamatrix_July_28.csv")
models <- read.csv("file:///home/gpauloski/git-repos/TACE/modelPredictions.csv")

#dataset[,"liver_BCLC"] <- as.numeric(factor(dataset[,"liver_BCLC"], levels=levels(factor(dataset[,"liver_BCLC"]))))
#dataset[,"liver_CLIP"] <- as.numeric(factor(dataset[,"liver_CLIP"], levels=levels(factor(dataset[,"liver_CLIP"]))))
#dataset[,"liver_TNM"] <- as.numeric(factor(dataset[,"liver_TNM"], levels=levels(factor(dataset[,"liver_TNM"]))))
#dataset[,"liver_Okuda"] <- as.numeric(factor(dataset[,"liver_Okuda"], levels=levels(factor(dataset[,"liver_Okuda"]))))

#dataset[,"liver_BCLC"] <- -1*dataset[,"liver_BCLC"]
#dataset[,"liver_CLIP"] <- -1*dataset[,"liver_CLIP"]
#dataset[,"liver_TNM"] <- -1*dataset[,"liver_TNM"]
#dataset[,"liver_Okuda"] <- -1*dataset[,"liver_Okuda"]

#dataset[,"liver_BCLC"] <- ifelse(dataset[,"liver_BCLC"] < 4, 2, 1)
#dataset[,"liver_CLIP"] <- ifelse(dataset[,"liver_CLIP"] < 3, 2, 1)
#dataset[,"liver_TNM"] <- ifelse(dataset[,"liver_TNM"] < 3, 2, 1)
#dataset[,"liver_Okuda"] <- ifelse(dataset[,"liver_Okuda"] < 2, 2, 1)

cindex <- function(time, pred) {
	nobs <- length(time)
	pairs <- combn(nobs,2)
	conc <- 0
	tie <- 0	
	
	for(i in 1:ncol(pairs)) {	
		if(time[pairs[1,i]] > time[pairs[2,i]]) {
			if(pred[pairs[1,i]] == pred[pairs[2,i]]) 
				tie = tie + 1
			if(pred[pairs[1,i]] > pred[pairs[2,i]]) 
				conc = conc + 1
		} else if(time[pairs[1,i]] < time[pairs[2,i]]) {
			if(pred[pairs[1,i]] == pred[pairs[2,i]]) 
				tie = tie + 1
			if(pred[pairs[1,i]] < pred[pairs[2,i]]) 
				conc = conc + 1
		} else tie = tie + 1
	}

	return((conc+(0.5*tie))/ncol(pairs))
} 

#cat("\nBCLC:", cindex(dataset[,"liver_TTP"],dataset[,"liver_BCLC"]))
#cat("\nCLIP:", cindex(dataset[,"liver_TTP"],dataset[,"liver_CLIP"]))
#cat("\nTNM:", cindex(dataset[,"liver_TTP"],dataset[,"liver_TNM"]))
#cat("\nOkuda:", cindex(dataset[,"liver_TTP"],dataset[,"liver_Okuda"]),"\n")

#cat("\nBCLC:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_BCLC"]))
#cat("\nCLIP:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_CLIP"]))
#cat("\nTNM:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_TNM"]))
#cat("\nOkuda:", estC(dataset[,"liver_TTP"],dataset[,"liver_CensorStatus"],dataset[,"liver_Okuda"]),"\n")

output <- data.frame(model=as.character(seq(1,ncol(models[,-1]),1)))
output <- cbind(output, c_index=seq(1,ncol(models[,-1]),1))
#output <- cbind(output, c_index2 = output$c_index)
for(i in 1:ncol(models[,-1])) {
	obs <- sort(c(which(models[,i+1]==1),which(models[,i+1]==2)))
	#output$c_index[i] <- estC(dataset[obs,"liver_TTP"], 
	#	dataset[obs,"liver_CensorStatus"],models[obs,i+1])
	output$model[i] <- colnames(models)[i+1]
	output$c_index[i] <- cindex(dataset[obs,"liver_TTP"],models[obs,i+1])
}

output$model <- colnames(models)[2:ncol(models)]
write.csv(output, file="model_c_index.csv")
cat("\nC-Index values saved to model_c_index.csv\n\n")



