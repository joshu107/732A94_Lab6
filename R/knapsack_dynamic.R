knapsack_dynamic <- function(x, W) {
  # The algorithm does not follow the pseudocode on wiki, but rather this 
  # explanation https://www.youtube.com/watch?v=8LusJS5-AGo . It is the same
  # but the matrix is rotated, plus it is explained how to identify actual
  # elements.
  
  n <- nrow(x)
  
  # Pre-allocate matrix
  m <- matrix(NA, nrow = n, ncol = W + 1)
  m[, 1] <- 0
  
  # Identify max values
  for (e in 1:n) { # up to this item is allowed to use
    for (w in 2:(W + 1)) { # max allowed weight in these iteration
      if (x$w[e] > w - 1) {
        # max(m[e - 1, w], 0) is needed, because if we are in the no values
        # in the row 0
        m[e, w] <- max(m[e - 1, w], 0)
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
  elements <- c()
  e <- n
  w <- W + 1
  done <- FALSE
  repeat {
    if (m[e, w] == 0) {
      break
    }
    
    if (m[e, w] > m[e - 1, w]) {
      elements <- c(elements, e)
      w <- w - x$w[e]
      e <- e - 1
    } else {
      e <- e - 1
    }
  }
  
  solution <- list()
  solution$value <- m[n, W + 1]
  solution$elements <- rev(elements)
  return(solution)
}