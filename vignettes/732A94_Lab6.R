## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github(repo = "Rchieve/GoogleGeoAPI",
#                           build_vignette = TRUE)

## ------------------------------------------------------------------------
library(Knapsack)

## ------------------------------------------------------------------------
generated_knapsack<-genKnapsack(200)

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

## ------------------------------------------------------------------------
knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)
