library(profvis)
library(rbenchmark)

knapsack_objects <- genKnapsack(2000)

# Performance evaluation of improvements without Rcpp

# knapsack_dynamic
nReplications <- 10

benchmark(
  knapsack_dynamic_wo(x = knapsack_objects[1:20,], W = 3500),
  replications = nReplications
)

benchmark(
  knapsack_dynamic_w(x = knapsack_objects[1:20,], W = 3500),
  replications = nReplications
)


# knapsack_greedy
nReplications <- 10
knapsack_objects <- genKnapsack(1000000)

benchmark(
  knapsack_greedy_wo(x = knapsack_objects[1:20000,], W = 350000),
  replications = nReplications
)

benchmark(
  knapsack_greedy_w(x = knapsack_objects[1:20000,], W = 350000),
  replications = nReplications
)








# library(Rcpp)
# cppFunction('double maxC(double a, double b) {
#   return(fmax(a, b));
# }')
# 
# 
