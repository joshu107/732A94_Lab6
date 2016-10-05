#' Greedy Heuristic approach for the knapsack problem with rcpp optimization
#' 
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#'
knapsack_greedy_rcpp <- function(x, W) {
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
  
  if (!isDfNonNegative(x)) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  if (!all(is.numeric(W), W > 0)) {
    stop("Weight value must be numeric and greater than 0.")
  }
  
  # Closure for storing elements
  elementsClosure <- function(n) {
    # Creates a closure to add new elements in place without copying an array
    # every time
    # 
    # Args:
    #   n: max length of a vector with elements
    
    elements <- numeric(length = n)
    i <- 1
    
    f <- function(element) {
      elements[i] <<- element
      i <<- i + 1
      elements
    }
    
    return(f)
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
  elements <- elementsClosure(n + 1) # we need one extra element
  # Pick elements with high ratio (higher in the data frame) first. If the weight limit
  # allows, pick the ones with smaller ratio
  for (e in 1:n) {
    if (xw[e] <= remainingWeight) {
      value <- value + xv[e]
      remainingWeight <- remainingWeight - xw[e]
      elements(xelement[e])
    } 
  }
  
  element <- elements(0) # call the closure to get the vector
  
  solution <- list()
  solution$value <- value
  solution$elements <- element[element != 0] # the vector is of length n + 1
                                             # but we need only non-zero elems
  
  return(solution)
}