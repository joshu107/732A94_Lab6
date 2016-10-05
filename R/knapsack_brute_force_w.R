knapsack_brute_force_w <- function(x, W) {
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
  
  weight <- value <- elements <- NA
    
  for (i in seq(from = 2, to = floor(W/min(x$w)))) {
    all_combinations <- combn(as.integer(row.names(x)), i)
    all_weights <- combn(x$w, i, sum)
    all_values <- combn(x$v, i, sum)
      
    possible_combination <- which(all_weights <= W)
    max_value <- which.max(all_values[possible_combination])
      
    tmp_weight <- all_weights[possible_combination[max_value]]
    tmp_value <- all_values[possible_combination[max_value]]
    tmp_elements <- all_combinations[, possible_combination[max_value]]
      
    if (any(tmp_value > value, is.na(value))) {
      weight <- tmp_weight
      value <- tmp_value
      elements <- tmp_elements
      if ((weight + min(x$w)) >= W) {
        return(res <- list(value = value, elements = elements))
      }
    }
    else {
      return(res <- list(value = value, elements = elements))
    }
  }
}




