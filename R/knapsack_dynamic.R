knapsack_dynamic <- function(x, W) {
  
  n <- nrow(x)
  
  # Pre-allocate matrix
  m <- matrix(NA, nrow = n, ncol = W + 1)
  m[, 1] <- 0
  
  # Identify max values
  for (e in 1:n) { # up to this item is allowed to use
    for (w in 2:(W + 1)) { # max allowed weight in these iteration
      if (x$w[e] > w - 1) {
        m[e, w] <- m[e - 1, w]
      } else {
        # max(m[e - 1, w - x$w[e]], 0) is needed, because if we are in the 
        # first row, then e - 1 returns numeric(0), but numeric(0) plus any
        # number is numeric(0). Thus, m[e, w - 1] will always be larger. Which 
        # is not what we want.
        m[e, w] <- max(m[e - 1, w], 
                       max(m[e - 1, w - x$w[e]], 0) + x$v[e])
      }
    }
  }
  
  # Indetify elements
  
  
  
  solution <- list()
  solution$value <- m[e, w]
  solution$elemnts <- "NOT IMPLEMENTED"
  return(solution)
}