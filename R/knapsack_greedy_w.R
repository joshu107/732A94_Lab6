knapsack_greedy_w <- function(x, W) {
  # Error handling
  if(!all(is.data.frame(x), 
          dim(x)[2] == 2,
          "v" %in% names(x),
          "w" %in% names(x)
  )) {
    stop('x has to be of the type data.frame with two columns.')
  }
  
  if(!all(is.numeric(x$w), is.numeric(x$v))) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  if (!all(x > 0)) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  if (!all(is.numeric(W), W > 0)) {
    stop("Weight value must be numeric and greater than 0.")
  }
  
  n <- nrow(x)
  
  # Add column with initial position
  x$element <- 1:n
  # Sort the data frame according to value-weight ratio in decreasing order
  x <- x[order(x$v / x$w, decreasing = TRUE), ]
  
  # Accessing vector is much faster than accessing a list (which a data frame is)
  # So we crate vectors for each column
  xv <- x$v
  xw <- x$w
  xelement <- x$element
  
  value <- 0
  remainingWeight <- W
  elements <- c()
  # Pick elements with high ratio (higher in the data frame) first. If the weight limit
  # allows, pick the ones with smaller ratio
  for (e in 1:n) {
    if (xw[e] <= remainingWeight) {
      value <- value + xv[e]
      remainingWeight <- remainingWeight - xw[e]
      elements <- c(elements, xelement[e])
    } 
  }
  
  solution <- list()
  solution$value <- value
  solution$elements <- elements
  
  return(solution)
}