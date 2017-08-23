# Usage: Rscript survivalforestscript.R
# Usage: source("survivalforestscript.R")
#

# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2017-08-23 11:12:38 x86_64-pc-linux-gnu 

# Rattle version 4.1.0 user 'fuentes'

# This log file captures all Rattle interactions as R commands. 

#Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
#library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2017-08-23 11:13:00 x86_64-pc-linux-gnu 

# Load the data.

crs$dataset <- read.csv("file:///rsrch1/ip/dtfuentes/github/RandomForestHCCResponse/datalocation/gmmdatamatrixfixed22Aug2017_Greg_edited.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
print(nrow(crs$dataset))

# subset on baseline only

crs$dataset <- subset(crs$dataset,liver_TimeID=="baseline")
print(nrow(crs$dataset))


#============================================================
# Rattle timestamp: 2017-08-23 11:49:29 x86_64-pc-linux-gnu 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 211 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) # 168 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 43 observations

# The following variable selections have been noted.

crs$input <- c("liver_gender", "liver_smoking", "liver_AFP", "liver_BCLC",
     "liver_Volume", "liver_Pre_DENOISE", "liver_Art_DENOISE", "liver_Ven_DENOISE",
     "liver_Del_DENOISE", "liver_Pre_GRADIENT", "liver_Art_GRADIENT", "liver_Ven_GRADIENT",
     "liver_Del_GRADIENT", "liver_Pre_MEAN_RADIUS_1", "liver_Art_MEAN_RADIUS_1", "liver_Ven_MEAN_RADIUS_1",
     "liver_Del_MEAN_RADIUS_1", "liver_Pre_SIGMA_RADIUS_1", "liver_Art_SIGMA_RADIUS_1", "liver_Ven_SIGMA_RADIUS_1",
     "liver_Del_SIGMA_RADIUS_1", "necrosis_Volume", "necrosis_Pre_DENOISE", "necrosis_Art_DENOISE",
     "necrosis_Ven_DENOISE", "necrosis_Del_DENOISE", "necrosis_Pre_GRADIENT", "necrosis_Art_GRADIENT",
     "necrosis_Ven_GRADIENT", "necrosis_Del_GRADIENT", "necrosis_Pre_MEAN_RADIUS_1", "necrosis_Art_MEAN_RADIUS_1",
     "necrosis_Ven_MEAN_RADIUS_1", "necrosis_Del_MEAN_RADIUS_1", "necrosis_Pre_SIGMA_RADIUS_1", "necrosis_Art_SIGMA_RADIUS_1",
     "necrosis_Ven_SIGMA_RADIUS_1", "necrosis_Del_SIGMA_RADIUS_1", "viable_Volume", "viable_Pre_DENOISE",
     "viable_Art_DENOISE", "viable_Ven_DENOISE", "viable_Del_DENOISE", "viable_Pre_GRADIENT",
     "viable_Art_GRADIENT", "viable_Ven_GRADIENT", "viable_Del_GRADIENT", "viable_Pre_MEAN_RADIUS_1",
     "viable_Art_MEAN_RADIUS_1", "viable_Ven_MEAN_RADIUS_1", "viable_Del_MEAN_RADIUS_1", "viable_Pre_SIGMA_RADIUS_1",
     "viable_Art_SIGMA_RADIUS_1", "viable_Ven_SIGMA_RADIUS_1", "viable_Del_SIGMA_RADIUS_1")

crs$numeric <- c("liver_AFP", "liver_Volume", "liver_Pre_DENOISE", "liver_Art_DENOISE",
     "liver_Ven_DENOISE", "liver_Del_DENOISE", "liver_Pre_GRADIENT", "liver_Art_GRADIENT",
     "liver_Ven_GRADIENT", "liver_Del_GRADIENT", "liver_Pre_MEAN_RADIUS_1", "liver_Art_MEAN_RADIUS_1",
     "liver_Ven_MEAN_RADIUS_1", "liver_Del_MEAN_RADIUS_1", "liver_Pre_SIGMA_RADIUS_1", "liver_Art_SIGMA_RADIUS_1",
     "liver_Ven_SIGMA_RADIUS_1", "liver_Del_SIGMA_RADIUS_1", "necrosis_Volume", "necrosis_Pre_DENOISE",
     "necrosis_Art_DENOISE", "necrosis_Ven_DENOISE", "necrosis_Del_DENOISE", "necrosis_Pre_GRADIENT",
     "necrosis_Art_GRADIENT", "necrosis_Ven_GRADIENT", "necrosis_Del_GRADIENT", "necrosis_Pre_MEAN_RADIUS_1",
     "necrosis_Art_MEAN_RADIUS_1", "necrosis_Ven_MEAN_RADIUS_1", "necrosis_Del_MEAN_RADIUS_1", "necrosis_Pre_SIGMA_RADIUS_1",
     "necrosis_Art_SIGMA_RADIUS_1", "necrosis_Ven_SIGMA_RADIUS_1", "necrosis_Del_SIGMA_RADIUS_1", "viable_Volume",
     "viable_Pre_DENOISE", "viable_Art_DENOISE", "viable_Ven_DENOISE", "viable_Del_DENOISE",
     "viable_Pre_GRADIENT", "viable_Art_GRADIENT", "viable_Ven_GRADIENT", "viable_Del_GRADIENT",
     "viable_Pre_MEAN_RADIUS_1", "viable_Art_MEAN_RADIUS_1", "viable_Ven_MEAN_RADIUS_1", "viable_Del_MEAN_RADIUS_1",
     "viable_Pre_SIGMA_RADIUS_1", "viable_Art_SIGMA_RADIUS_1", "viable_Ven_SIGMA_RADIUS_1", "viable_Del_SIGMA_RADIUS_1")

crs$categoric <- c("liver_gender", "liver_smoking", "liver_BCLC")

crs$target  <- "liver_TTP"
crs$risk    <- "liver_CensorModality"
crs$ident   <- NULL
crs$ignore  <- c("studyUID.liver_dataID", "liver_LesionNumber", "liver_LobeLocation", "liver_SegmentLocation", "liver_ChemoUsed", "liver_WeeksBetweenBLandTACE", "liver_TreatmentDose", "liver_hepatitis", "liver_age", "liver_alcohol", "liver_Cirrhosis", "liver_Pathology", "liver_Vascular", "liver_Metastasis", "liver_Lymphnodes", "liver_Thrombosis", "liver_FirstLineTherapy", "liver_MVIStatus", "liver_PNPLA3", "liver_PNPLA73", "liver_pnpl2", "liver_TTPTraining", "liver_Survival", "liver_LabelTraining", "liver_AFPGroup", "liver_CLIPScore", "liver_CLIP", "liver_Okuda", "liver_TNM", "liver_CensorStatus", "liver_FirstTACESession", "liver_StatusShift", "liver_StatusDate", "liver_deathdate", "liver_CensorDead", "liver_DxDateBiopsy", "liver_NoInfection", "liver_HBValone0", "liver_HCValone0", "liver_age60", "liver_SexM1F0", "liver_Smoking_status", "liver_Alcohol_status", "liver_fhx_can", "liver_fhx_livc", "liver_Diabetes_status", "liver_Personal_Hx", "liver_Evidence_of_cirrhosis", "liver_Poorly_differentiated", "liver_tumor_nodul", "liver_Vascular_invasion", "liver_Mets", "liver_LNs", "liver_PV_Thrombosis", "liver_T_involvment50", "liver_AFP_group400", "liver_CLIP_02_2_36_1", "liver_OKUDA1_23", "liver_TNM12_34", "liver_BCLC_AB2_CD1", "liver_TimeID", "liver_MorshidQA", "liver_Status", "liver_StudyUID", "liver_labellocation", "liver_Pre_RAWIMAGE", "liver_Art_RAWIMAGE", "liver_Ven_RAWIMAGE", "liver_Del_RAWIMAGE", "liver_Pre_ATROPOS_GMM_POSTERIORS1", "liver_Art_ATROPOS_GMM_POSTERIORS1", "liver_Ven_ATROPOS_GMM_POSTERIORS1", "liver_Del_ATROPOS_GMM_POSTERIORS1", "liver_Pre_ATROPOS_GMM_POSTERIORS2", "liver_Art_ATROPOS_GMM_POSTERIORS2", "liver_Ven_ATROPOS_GMM_POSTERIORS2", "liver_Del_ATROPOS_GMM_POSTERIORS2", "liver_Pre_ATROPOS_GMM_POSTERIORS3", "liver_Art_ATROPOS_GMM_POSTERIORS3", "liver_Ven_ATROPOS_GMM_POSTERIORS3", "liver_Del_ATROPOS_GMM_POSTERIORS3", "liver_Pre_ATROPOS_GMM_LABEL1_DISTANCE", "liver_Art_ATROPOS_GMM_LABEL1_DISTANCE", "liver_Ven_ATROPOS_GMM_LABEL1_DISTANCE", "liver_Del_ATROPOS_GMM_LABEL1_DISTANCE", "liver_Pre_MEAN_RADIUS_3", "liver_Art_MEAN_RADIUS_3", "liver_Ven_MEAN_RADIUS_3", "liver_Del_MEAN_RADIUS_3", "liver_Pre_MEAN_RADIUS_5", "liver_Art_MEAN_RADIUS_5", "liver_Ven_MEAN_RADIUS_5", "liver_Del_MEAN_RADIUS_5", "liver_Pre_SIGMA_RADIUS_3", "liver_Art_SIGMA_RADIUS_3", "liver_Ven_SIGMA_RADIUS_3", "liver_Del_SIGMA_RADIUS_3", "liver_Pre_SIGMA_RADIUS_5", "liver_Art_SIGMA_RADIUS_5", "liver_Ven_SIGMA_RADIUS_5", "liver_Del_SIGMA_RADIUS_5", "liver_Pre_SKEWNESS_RADIUS_1", "liver_Art_SKEWNESS_RADIUS_1", "liver_Ven_SKEWNESS_RADIUS_1", "liver_Del_SKEWNESS_RADIUS_1", "liver_Pre_SKEWNESS_RADIUS_3", "liver_Art_SKEWNESS_RADIUS_3", "liver_Ven_SKEWNESS_RADIUS_3", "liver_Del_SKEWNESS_RADIUS_3", "liver_Pre_SKEWNESS_RADIUS_5", "liver_Art_SKEWNESS_RADIUS_5", "liver_Ven_SKEWNESS_RADIUS_5", "liver_Del_SKEWNESS_RADIUS_5", "necrosis_Pre_RAWIMAGE", "necrosis_Art_RAWIMAGE", "necrosis_Ven_RAWIMAGE", "necrosis_Del_RAWIMAGE", "necrosis_Pre_ATROPOS_GMM_POSTERIORS1", "necrosis_Art_ATROPOS_GMM_POSTERIORS1", "necrosis_Ven_ATROPOS_GMM_POSTERIORS1", "necrosis_Del_ATROPOS_GMM_POSTERIORS1", "necrosis_Pre_ATROPOS_GMM_POSTERIORS2", "necrosis_Art_ATROPOS_GMM_POSTERIORS2", "necrosis_Ven_ATROPOS_GMM_POSTERIORS2", "necrosis_Del_ATROPOS_GMM_POSTERIORS2", "necrosis_Pre_ATROPOS_GMM_POSTERIORS3", "necrosis_Art_ATROPOS_GMM_POSTERIORS3", "necrosis_Ven_ATROPOS_GMM_POSTERIORS3", "necrosis_Del_ATROPOS_GMM_POSTERIORS3", "necrosis_Pre_ATROPOS_GMM_LABEL1_DISTANCE", "necrosis_Art_ATROPOS_GMM_LABEL1_DISTANCE", "necrosis_Ven_ATROPOS_GMM_LABEL1_DISTANCE", "necrosis_Del_ATROPOS_GMM_LABEL1_DISTANCE", "necrosis_Pre_MEAN_RADIUS_3", "necrosis_Art_MEAN_RADIUS_3", "necrosis_Ven_MEAN_RADIUS_3", "necrosis_Del_MEAN_RADIUS_3", "necrosis_Pre_MEAN_RADIUS_5", "necrosis_Art_MEAN_RADIUS_5", "necrosis_Ven_MEAN_RADIUS_5", "necrosis_Del_MEAN_RADIUS_5", "necrosis_Pre_SIGMA_RADIUS_3", "necrosis_Art_SIGMA_RADIUS_3", "necrosis_Ven_SIGMA_RADIUS_3", "necrosis_Del_SIGMA_RADIUS_3", "necrosis_Pre_SIGMA_RADIUS_5", "necrosis_Art_SIGMA_RADIUS_5", "necrosis_Ven_SIGMA_RADIUS_5", "necrosis_Del_SIGMA_RADIUS_5", "necrosis_Pre_SKEWNESS_RADIUS_1", "necrosis_Art_SKEWNESS_RADIUS_1", "necrosis_Ven_SKEWNESS_RADIUS_1", "necrosis_Del_SKEWNESS_RADIUS_1", "necrosis_Pre_SKEWNESS_RADIUS_3", "necrosis_Art_SKEWNESS_RADIUS_3", "necrosis_Ven_SKEWNESS_RADIUS_3", "necrosis_Del_SKEWNESS_RADIUS_3", "necrosis_Pre_SKEWNESS_RADIUS_5", "necrosis_Art_SKEWNESS_RADIUS_5", "necrosis_Ven_SKEWNESS_RADIUS_5", "necrosis_Del_SKEWNESS_RADIUS_5", "viable_Pre_RAWIMAGE", "viable_Art_RAWIMAGE", "viable_Ven_RAWIMAGE", "viable_Del_RAWIMAGE", "viable_Pre_ATROPOS_GMM_POSTERIORS1", "viable_Art_ATROPOS_GMM_POSTERIORS1", "viable_Ven_ATROPOS_GMM_POSTERIORS1", "viable_Del_ATROPOS_GMM_POSTERIORS1", "viable_Pre_ATROPOS_GMM_POSTERIORS2", "viable_Art_ATROPOS_GMM_POSTERIORS2", "viable_Ven_ATROPOS_GMM_POSTERIORS2", "viable_Del_ATROPOS_GMM_POSTERIORS2", "viable_Pre_ATROPOS_GMM_POSTERIORS3", "viable_Art_ATROPOS_GMM_POSTERIORS3", "viable_Ven_ATROPOS_GMM_POSTERIORS3", "viable_Del_ATROPOS_GMM_POSTERIORS3", "viable_Pre_ATROPOS_GMM_LABEL1_DISTANCE", "viable_Art_ATROPOS_GMM_LABEL1_DISTANCE", "viable_Ven_ATROPOS_GMM_LABEL1_DISTANCE", "viable_Del_ATROPOS_GMM_LABEL1_DISTANCE", "viable_Pre_MEAN_RADIUS_3", "viable_Art_MEAN_RADIUS_3", "viable_Ven_MEAN_RADIUS_3", "viable_Del_MEAN_RADIUS_3", "viable_Pre_MEAN_RADIUS_5", "viable_Art_MEAN_RADIUS_5", "viable_Ven_MEAN_RADIUS_5", "viable_Del_MEAN_RADIUS_5", "viable_Pre_SIGMA_RADIUS_3", "viable_Art_SIGMA_RADIUS_3", "viable_Ven_SIGMA_RADIUS_3", "viable_Del_SIGMA_RADIUS_3", "viable_Pre_SIGMA_RADIUS_5", "viable_Art_SIGMA_RADIUS_5", "viable_Ven_SIGMA_RADIUS_5", "viable_Del_SIGMA_RADIUS_5", "viable_Pre_SKEWNESS_RADIUS_1", "viable_Art_SKEWNESS_RADIUS_1", "viable_Ven_SKEWNESS_RADIUS_1", "viable_Del_SKEWNESS_RADIUS_1", "viable_Pre_SKEWNESS_RADIUS_3", "viable_Art_SKEWNESS_RADIUS_3", "viable_Ven_SKEWNESS_RADIUS_3", "viable_Del_SKEWNESS_RADIUS_3", "viable_Pre_SKEWNESS_RADIUS_5", "viable_Art_SKEWNESS_RADIUS_5", "viable_Ven_SKEWNESS_RADIUS_5", "viable_Del_SKEWNESS_RADIUS_5", "viable_LEFTLUNGDISTANCE", "viable_RIGHTLUNGDISTANCE", "viable_LANDMARKDISTANCE0", "viable_LANDMARKDISTANCE1", "viable_LANDMARKDISTANCE2", "viable_HESSOBJ", "viable_NORMALIZEDDISTANCE")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-08-23 11:50:27 x86_64-pc-linux-gnu 

# Survival Model 

# Require the survival package.

library(survival, quietly=TRUE)
# untar(download.packages(pkgs = "survival", destdir = "./survivalsrc", type = "source",repos='http://cran.us.r-project.org')[,2])

# Build the Survival model.

crs$survival <- coxph(Surv(liver_TTP, liver_CensorModality) ~ liver_gender + liver_smoking + liver_AFP + liver_BCLC + liver_Volume + liver_Pre_DENOISE + liver_Art_DENOISE + liver_Ven_DENOISE + liver_Del_DENOISE + liver_Pre_GRADIENT + liver_Art_GRADIENT + liver_Ven_GRADIENT + liver_Del_GRADIENT + liver_Pre_MEAN_RADIUS_1 + liver_Art_MEAN_RADIUS_1 + liver_Ven_MEAN_RADIUS_1 + liver_Del_MEAN_RADIUS_1 + liver_Pre_SIGMA_RADIUS_1 + liver_Art_SIGMA_RADIUS_1 + liver_Ven_SIGMA_RADIUS_1 + liver_Del_SIGMA_RADIUS_1 + necrosis_Volume + necrosis_Pre_DENOISE + necrosis_Art_DENOISE + necrosis_Ven_DENOISE + necrosis_Del_DENOISE + necrosis_Pre_GRADIENT + necrosis_Art_GRADIENT + necrosis_Ven_GRADIENT + necrosis_Del_GRADIENT + necrosis_Pre_MEAN_RADIUS_1 + necrosis_Art_MEAN_RADIUS_1 + necrosis_Ven_MEAN_RADIUS_1 + necrosis_Del_MEAN_RADIUS_1 + necrosis_Pre_SIGMA_RADIUS_1 + necrosis_Art_SIGMA_RADIUS_1 + necrosis_Ven_SIGMA_RADIUS_1 + necrosis_Del_SIGMA_RADIUS_1 + viable_Volume + viable_Pre_DENOISE + viable_Art_DENOISE + viable_Ven_DENOISE + viable_Del_DENOISE + viable_Pre_GRADIENT + viable_Art_GRADIENT + viable_Ven_GRADIENT + viable_Del_GRADIENT + viable_Pre_MEAN_RADIUS_1 + viable_Art_MEAN_RADIUS_1 + viable_Ven_MEAN_RADIUS_1 + viable_Del_MEAN_RADIUS_1 + viable_Pre_SIGMA_RADIUS_1 + viable_Art_SIGMA_RADIUS_1 + viable_Ven_SIGMA_RADIUS_1 + viable_Del_SIGMA_RADIUS_1,
      data=crs$dataset[crs$train,c(crs$input, crs$risk, crs$target)])

# Print the results of the modelling.

summary(crs$survival)

# Time taken: 0.04 secs


#============================================================
# Rattle timestamp: 2017-08-23 11:22:50 x86_64-pc-linux-gnu 

# Score a dataset. 

# Obtain class for the Survival model on gmmdatamatrixfixed22Aug2017_Greg_edited.csv [test].

crs$pr <- survival:::survmean(survfit(crs$survival, na.omit(crs$dataset[crs$test, c(crs$input)])), scale=1, rmean="none")[[1]][,5]

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$test,], select=c("liver_TTP"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="./test_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2017-08-23 11:24:51 x86_64-pc-linux-gnu 

# Score a dataset. 

# Obtain class for the Survival model on gmmdatamatrixfixed22Aug2017_Greg_edited.csv [test].

crs$pr <- survival:::survmean(survfit(crs$survival, na.omit(crs$dataset[crs$test, c(crs$input)])), scale=1, rmean="none")[[1]][,5]

# Extract the relevant variables from the dataset.

sdata <- crs$dataset[crs$test,]

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="./test_score_all.csv", row.names=FALSE)


# compare to @gpauloski c-index calculations on test set

#options(na.action=na.exclude)
#fit <- coxph(Surv(time, status) ~ ph.ecog + age + sex, lung)
#survConcordance(Surv(time, status) ~predict(fit), lung)

options(na.action=na.exclude)
testdata  = crs$dataset[crs$test, c(crs$input, crs$target, crs$risk)  ]
survConcordance(Surv(liver_TTP, liver_CensorModality)  ~predict(crs$survival,testdata  ),testdata  )

# Greg's C-index
source("cindex.R")
#   install.packages("compareC",repos='http://cran.us.r-project.org')
# cindex(actual values, predicted values)
c_ind <- cindex(crs$dataset[crs$test,crs$target], crs$pr)
cat("C-index =", c_ind, "\n")
