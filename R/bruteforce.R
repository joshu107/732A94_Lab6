# Generating input --------------------------------------------------------

set.seed(42)
n <- 3
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


# reduce <- function(x, W) {
#   x$id <- seq(1:nrow(x))
#   x <- x[x[, 1] <= W, ]
#   x
# }

# Brute force approach ----------------------------------------------------

knapsack_brute_force <- function(x, W) {
  if(!any(is.data.frame(x), dim(x)[2] == 2)) {
    stop('x has to be of the type data.frame with two columns.')
  }
  
  if(!all(is.numeric(knapsack_objects), knapsack_objects > 0)) {
    stop('Values in the data.frame must be numeric and greater than 0.')
  }
  
  # Only consider items which weigh less or equal than W.
  # x <- reduce(x, W)
  
  # Create a data.frame which contains all possible combinations
  n <- nrow(x)
  if (n < 2^31) {
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
  value <- apply(df, 1, function(m) round(sum(m * x$v)))
  
  df$weight <- weight
  df$value <- value
  
  # Filter columns and return the combination with the highest value
  comb <- df[df$weight <= W, ]
  best_combination <- comb[max(comb$value) == comb$value, ]
  
  res <- list(value = best_combination$value, 
              elements = which(best_combination == 1))
  res
  
}


