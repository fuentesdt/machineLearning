Usage for datamatrix modeling

R packages: c("caret", "kernlab", "knitr", "e1071", "magrittr", "rpart", "nnet", "parallel",
"randomForest", "xgboost", "Boruta", "leaps", "MASS", "ranger", "cluster", "subselect", "corrplot", "gridExtra","pROC")

In terminal (in windows find Rscript.exe): sample usage


Rscript -e "rmarkdown::render('datamatrixModeling_binary.Rmd', 
                               output_file = NULL, 
                               params = list( csvPath = 'data.csv',
                                              target = 'liver_TTP28',
                                              inputs =  readRDS("liver_inputs_continuous.RDS")
                                              leaveOneOut: FALSE
                                            )
                              )"



if inputs is omitted, all non-target columns are used.


inputs needs R syntax: example c("width", "height", "volume").
An RDS fine with lists of input names makes it easy. see ?saveRDS


full list of parameters:

 csvPath:          gmmdatamatrixfixed22Aug2017_Greg_edited_EDGadd.csv
 target:           liver_TTP28
 inputs:           !r readRDS("liver_inputs_continuous.RDS")
 leaveOneOut:      !r FALSE
 rescale:          !r FALSE
 removeCorrelated: !r TRUE
 semisupervised:   !r FALSE 
 kClusters:        !r as.numeric(9)
 genetic:          !r FALSE
 boruta:           !r TRUE
 univariate:       !r TRUE

rescale transforms columns to mean/varoance 0/1, sumisupervised and genetic are not fully implemented

leaveOneOut toggles k-fold cross validation or leave-one-out CV.


Note: some table rendering requires a TeX compiler and may need to install packages