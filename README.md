# machineLearning

## tace_v2.R usage instructions:

Builds and predicts classification of data using Random Forest, Support Vector Machine, and XGBoost.
Builds each model using a combination of baseline variables and set of imaging data variables.
    
    Baseline = BCLC, CLIP, Okuda, TNM    
    Image data sets created using volumes, stepwise regression, and exhaustive regression

  ### Packages
    Required: randomForest, e1071, xgboost, leaps, MASS, caret
    Optional: utils.R script found in this repo. Only used when creating scatter plots with highest correlated data (default disabled)

  ### Input: paramters found at beginning of script
    dataset <- csv file of data matrix
    stepwise <- TRUE if you want to include a stepwise selection of the image data
    exhaustive <- TRUE if you want to include an exhaustive selction of image data. Default = FALSE due to time to compute
    outputFile <- name of csv file that will be created containing leave-one-out predictions for each model created

  ### Important notes!
    varMain = string vector of the baseline variables
    varImg = list of 3 vectors, one for each of the image data sets (volume, stepwise, exhaustive). If one of these vectors is left NULL (ex: by leaving the exhaustive parameter as FALSE) then the script will ignore it when building the models. 
    This script assumes that the image data variables start at the column "liver_Volume" and ends at the last column in the data frame.
      The script will then remove any columns in the imgData set that are empty leaving a vector called imgData containing the name of each column in the original data matrix with imaging data. 
      This imgData set is then used in the stepwise and exhaustive subset selection.
    To get c-indexes, run cinde.R as its own script. In the script is a line to import the modelPredictions.csv output by tace_v2.R.
      cindex.R outputs its own csv containing cindex value for each model in modelPredictions.csv

  ### Program Structure
    import data
    Create input lists from image data using volumes, stepwise, and exhaustive methods
    for each i in varMain
      for each j in varImg
        model_input = varMain(i) + varImg(j)
        for each k in 1:numObservations
          build rf, svm, xgb model using data(-k) to train
          predict data(k) using models
        end for
      end for
    end for
