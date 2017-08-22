library(ggplot2, quietly=TRUE)
library(GGally, quietly=TRUE)
library(maptree, quietly=TRUE)

plotTree <- function(dTree, dataset, target, input, 
    filename="treeScatterMatrix") {

  # Create pdf to print diagrams to
  pdf(width=11, height=8.5, file=filename)
  
  # Check if necessary parameters are missing
  if(missing(dataset)) {
    print("Missing dataset! Quiting function.")
    stop()
  } else if(missing(target)) {
    print("Missing target of dataset! Quiting function.")
    stop()
  }
 
  # If no tree object passed to function, build a decision tree from dataset
  if(missing(dTree)) {
    # Check if vector 'input' of variable names is missing
    if(missing(input)) { 
      print("Must provide vector of input variables to build tree!")
      stop()
    }
    else {
      # Build decision tree and store in dTree
      library(tree, quietly=TRUE)
      library(maptree, quietly=TRUE)
      colnames(dataset)[which(colnames(dataset) == target)] <- "target"
      target <- "target"
      dTree <- tree(target~., data=dataset[,c(target,input)])
      pred <- predict(dTree, dataset[,input], type="class")
      treeError <- (length(which(dataset[,target] != pred))/length(pred))*100
      cat("\nDecision tree created (training set error = ", treeError, ")\n",
          sep="")
    }
  }

  # Draw decision tree diagram
  print("Drawing decision tree diagram")
  draw.tree(dTree, cex=0.5, nodeinfo=TRUE)

  # Create vector of colors for each observation in dataframe based on factor
  # level. Only 5 colors are listed here so factor levels must be between 
  # 1 and 5.
  colourList <- c("red", "green", "purple3", "blue", "black")
  plotColour <- NULL
  for(i in 1:nrow(dataset)) {
     if((as.numeric(dataset[i,target]) < 1) || 
         (as.numeric(dataset[i,target]) > 5)) {
       print("Target must be a factor with levels between 1 and 5!")
       stop()
     }
     plotColour <- c(plotColour, colourList[as.numeric(dataset[i,target])]) 
  }

  # Label each node with its parent node. The tree structure by default
  # provides no way to get the child/or parent of a node. In order to draw a 
  # scatter plot matrix with children of a node, each node in the tree has to 
  # be labeled first

  # Add columns for node number and empty vector for parent node number
  dTree$frame <- cbind(dTree$frame, node = as.numeric(rownames(dTree$frame)),
      parent = integer(nrow(dTree$frame)))

  # Get list of nodes, this will be used to keep track of which nodes have 
  # been labeled with a parent
  nodes <- dTree$frame$node

  # Loop through all nodes in tree and find it's parent node
  for(i in 1:nrow(dTree$frame)) {

    # Ignore the first node (root node) because it cannot have a parent
    if(dTree$frame$node[i] != 1) {

      # Set parent of current node equal to the node before the current node in
      # 'nodes'
      dTree$frame$parent[i] <- nodes[which(nodes == dTree$frame$node[i])-1]

      # If current node is a leaf, remove this node from 'nodes'
      # This is necessary because a leaf cannot be a parent so removing it from
      # 'nodes' after assigining its parent prevents it from being used as a 
      # parent in later operations
      if(dTree$frame$var[i] == "<leaf>") {
        nodes <- nodes[-(which(nodes == dTree$frame$node[i]))]
      }

      # If the current node's parent now has two children, remove that parent
      # from 'nodes' since each parent has exactly two children in binary tree
      # This prevents that parent from being assigned another child in later
      # iterations
      if(length(which(dTree$frame$parent == dTree$frame$parent[i])) == 2) {
        nodes <- nodes[-(which(nodes == dTree$frame$parent[i]))]
      }
    }
  }

  # Get list of observations left in each node after splits

  # Get list of parent nodes and remove the first index because it is 0
  parents <- unique(dTree$frame$parent)
  parents <- parents[2:length(parents)]

  # Initialize empty list equal to number of nodes to store vectors of obs in
  # each node
  nodeObs <- list(seq(1, nrow(dataset), 1))
  for(i in 1:(nrow(dTree$frame)-1)) nodeObs <- c(nodeObs,list(vector()))

  # For each parent node, split data in node and assign split data to the node's
  # children
  for(i in 1:length(parents)) {
    parent <- parents[i]
    parentIndex <- which(dTree$frame$node == parent)
    split <- as.numeric(substring(dTree$frame$splits[parentIndex, 1], 2))
    var <- as.character(dTree$frame$var[parentIndex])
    children <- which(dTree$frame$parent == parent)
    # Assign vector of data obs index less than split to left child and greater
    # than split to right child
    nodeObs[[children[1]]] <- nodeObs[[parentIndex]][
        is.element(nodeObs[[parentIndex]], which(dataset[, var] < split))]
    nodeObs[[children[2]]] <- nodeObs[[parentIndex]][
        is.element(nodeObs[[parentIndex]], which(dataset[, var] > split))]
  }

  # Convert split variable names from factor to string
  dTree$frame$var <- as.character(dTree$frame$var)

  # For each parent node in tree, draw scatter plot matrix with parent and 
  # its children
  for(i in 1:length(parents)) {
    cat("Drawing scatter plot matrix for node", parents[i], "\n")
    # Get indexes of nodes being used, colour of data points in node, parent
    # split line
    parent <- parents[i]
    parentIndex <- which(dTree$frame$node == parent)
    leftChild <- which(dTree$frame$parent == parent)[1]
    rightChild <- which(dTree$frame$parent == parent)[2]
    colour <- plotColour[nodeObs[[parentIndex]]]
    parentLine <- as.numeric(substring(dTree$frame$splits[parentIndex, 1], 2))
    leaf <- seq(1, length(nodeObs[[parentIndex]]), 1)

    # There are 4 possible type of parent nodes (two leafs, no leafs, left
    # leaf, and right leaf). For each type of node, create its plotData frame
    # from vars in parent and children and get the decision boundaries
    if(dTree$frame$var[leftChild] == "<leaf>" & 
        dTree$frame$var[rightChild] == "<leaf>") {
      plotData <- data.frame(cbind(leaf, dataset[nodeObs[[parentIndex]], 
          as.character(dTree$frame$var[parentIndex])], leaf))
      colnames(plotData)[2] <- dTree$frame$var[parentIndex]
      leftChildLine <- 0
      rightChildLine <- 0
    } else if(dTree$frame$var[leftChild] == "<leaf>") {
      plotData <- data.frame(cbind(leaf, dataset[nodeObs[[parentIndex]], 
          as.character(c(dTree$frame$var[parentIndex], 
          dTree$frame$var[rightChild]))]))
      colnames(plotData)[2:3] <- dTree$frame$var[c(parentIndex, rightChild)]
      leftChildLine <- 0
      rightChildLine <- as.numeric(substring(
          dTree$frame$splits[rightChild, 1], 2))
    } else if(dTree$frame$var[rightChild] == "<leaf>") {
      plotData <- data.frame(cbind(dataset[nodeObs[[parentIndex]], 
          as.character(c(dTree$frame$var[leftChild], 
          dTree$frame$var[parentIndex]))], leaf))
      colnames(plotData)[1:2] <- dTree$frame$var[c(leftChild, parentIndex)]
      leftChildLine <- as.numeric(substring(
          dTree$frame$splits[leftChild, 1], 2))
      rightChildLine <- 0
    } else {
      plotData <- dataset[nodeObs[[parentIndex]], 
          as.character(c(dTree$frame$var[leftChild],
          dTree$frame$var[parentIndex], dTree$frame$var[rightChild]))]
      leftChildLine <- as.numeric(substring(
          dTree$frame$splits[leftChild, 1], 2))
      rightChildLine <- as.numeric(substring(
          dTree$frame$splits[rightChild, 1], 2))
    }

    # Make names of plotData easier to read
    colnames(plotData)[1] <- paste(colnames(plotData)[1], "(left leaf)")
    colnames(plotData)[2] <- paste(colnames(plotData)[2], "(center node)")
    colnames(plotData)[3] <- paste(colnames(plotData)[3], "(right leaf)")

    # Build scatter plot matrix
    plot <- ggpairs(plotData, title=paste("Node:", parent),
        lower=list(continuous=wrap("points", colour=colour, size=0.7)))

    # Get plot from matrix, add decision boundary lines, and put back in matrix
    plot$plots[[4]] <- getPlot(plot, 2, 1) + 
        geom_hline(yintercept=parentLine) + 
        geom_vline(xintercept=leftChildLine)
    plot$plots[[8]] <- getPlot(plot, 3, 2) + 
        geom_hline(yintercept=rightChildLine) +
        geom_vline(xintercept=parentLine)
    plot$plots[[1]] <- getPlot(plot, 1, 1) +
        geom_vline(xintercept=leftChildLine)
    plot$plots[[5]] <- getPlot(plot, 2, 2) +
        geom_vline(xintercept=parentLine)
    plot$plots[[9]] <- getPlot(plot, 3, 3) +
        geom_vline(xintercept=rightChildLine)

    print(plot)
  }
  cat("Decision tree scatter plots saved to", filename, "\n")
}
