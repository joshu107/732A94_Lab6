knapsack_dynamic <- function(x, W) {
  
  
  # Following the pseudocode on wiki.
  # Nothe that there they use an array which has a 0-th element. We don't, 
  # thus we start i from 2, conrinue it to n+1 and when we access elements of x
  # we substract 1 from i.
  n <- nrow(x)
  
  m <- matrix(NA, nrow = n + 1, ncol = W)
  m[1, ] <- 0
  
  for (i in 1:n+1) {
    for (j in 1:W) {
      if(x$w[i - 1] > j) {
        m[i, j] <- m[i - 1, j]
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - x$w[i - 1]] + x$v[i - 1])
      }
    }
  }
  
  res <- list()
  res$value <- m[i, j]
  res$elemnts <- "NOT IMPLEMENTED YET"
  return(res)
}