knapsack_brute_force_wo <- function(x, W) {
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
  
  # Only consider items which weigh less or equal than W.
  # 
  # x <- reduce(x, W)
  
  # Create a data.frame which contains all possible combinations
  n <- nrow(x)
  if (n < 31) {
    # Generate a war matrix where each column represents a combination, each row
    # represents an item. Transpose. Remove extra columns (with only zeros).
    # Convert to logical. Convert to data frame
    df <- as.data.frame(
      matrix(
        as.logical(
          t(sapply(1:2^n, FUN = function(x) {intToBits(x)}))[, 1:n]),
        ncol = n)
    )
  } else {
    df <- expand.grid(lapply(row.names(x), function(n) seq.int(0,1)))
  }
  
  # Add column with the weight and the value of each combination
  weight <- apply(df, 1, function(n) sum(n * x$w))
  value <- apply(df, 1, function(m) sum(m * x$v))
  
  df$weight <- weight
  df$value <- value
  
  # Filter columns and return the combination with the highest value
  comb <- df[df$weight <= W, ]
  best_combination <- comb[max(comb$value) == comb$value, ]
  
  res <- list(value = best_combination$value, 
              elements = which(best_combination == 1))
  res
  
}






