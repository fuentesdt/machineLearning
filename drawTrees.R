# Draws trees from random forest model
#
# To use this function:
#	(1) place this file in your working directory
#	(2) use the source function in your script to import this code
#		ex) source("drawTrees.R")
#	(3) call the function
#		ex) drawTrees(randomForestModel) 
#
# Uses the reprtree package by araastat
#   https://github.com/araastat/reprtree
# Checks if required packages are installed and installs missing packages
# 
# If installing reprtree fails, it may be because OpenSSL is not installed
# 'devtools' requires the git2r package which can only be installed using OpenSSL
# See error messages in console for more information
#
# INPUT:
#	model : randomForest model from package 'randomForest'
#		random forest models from other packages may not work
#	filename : name of pdf file containing tree diagrams
#		   default = "TreeDiagrams.pdf"
# OUTPUT:
#	PDF file containing 

drawTrees <- function(model, filename="TreeDiagrams.pdf") {
	# Check if reprtree packages installed. If not, install reprtree
	# and its dependencies
	if(!('reprtree' %in% rownames(installed.packages()))) {
		print("Installing reprtree package...")

		# Check if dependencies are installed. If not, install them
		options(repos="http://cran.rstudio.org")
		have.packages <- installed.packages()
		cran.packages <- c('devtools','plotrix','randomForest','tree')
		to.install <- setdiff(cran.packages, have.packages[,1])
		if(length(to.install)>0) install.packages(to.install)
	
		# Install reprtree
		library(devtools)
		install_github('araastat/reprtree')
		
		# Check if installation worked. If not, exit function
		if('reprtree' %in% rownames(installed.packages())) {
			print("Reprtree installed")
		} else {
			stop("Installation failed! Try manually installing reprtree package\n")
		}
	}
	
	library(reprtree, quietly=T)

	cat("\nDrawing tree diagrams...\n")

	# Set filename, width, and height of output file
	pdf(file=filename, width=11, height=8.5)
	
	# For each tree in model, draw tree diagram and print to pdf file
	for(i in 1:model$ntree) reprtree:::plot.getTree(model,k=i)

	cat("Printed diagrams for", model$ntree, "tree(s) to \"", filename, "\"\n")
}
