# Inputs------------------------------------------------------------------------
context("knapsack_greedy inputs")

x <- genKnapsack(10)
# Test for NOT numeric and greater than zero
xNonNumeric1 <- data.frame(w = c("1", "2"), v = c("4", "5"))
xNonNumeric2 <- data.frame(w = c("1", "2"), v = c(4, 5))
xNonNumeric3 <- data.frame(w = c(1, 2), v = c("4", "5"))
xNonNumeric4 <- data.frame(w = c(1, 2), v = c(TRUE, FALSE))
xNonNumeric5 <- data.frame(w = c(FALSE, TRUE), v = c(4, 5))
xLeZero1 <- data.frame(w = c(1, 2), v = c(4, 0))
xLeZero2 <- data.frame(w = c(1, 2), v = c(4, -1))
xLeZero3 <- data.frame(w = c(0, 2), v = c(4, 5))
xLeZero4 <- data.frame(w = c(-1, 2), v = c(4, 5))
wNonNumeric <- "2"
wLeZero1 <- 0
wLeZero2 <- -1
# Test for NOT data frame with 2 columns
xNotDf1 <- c(1, 2, 3)
xNotDf2 <- list(w = c(1, 2, 3), v = c(1, 2, 3))
xNotDf3 <- "a"
xNotDf4 <- TRUE
xWrongColumns1 <- data.frame(a = c(1, 2), b = c(4, 5))
xWrongColumns2 <- data.frame(w = c(1, 2), v = c(4, 5), z = c(6, 7))

test_that("Inputs", {
  expect_error(knapsack_greedy(x = xNonNumeric1, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xNonNumeric2, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xNonNumeric3, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xNonNumeric4, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xNonNumeric5, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xLeZero1, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xLeZero2, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xLeZero3, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xLeZero4, W = 1), 
               "Values in the data.frame must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = x, W = wNonNumeric), 
               "Weight value must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = x, W = wLeZero1), 
               "Weight value must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = x, W = wLeZero2), 
               "Weight value must be numeric and greater than 0.")
  expect_error(knapsack_greedy(x = xNotDf1, W = 1), 
               "x has to be of the type data.frame with two columns.")
  expect_error(knapsack_greedy(x = xNotDf2, W = 1), 
               "x has to be of the type data.frame with two columns.")
  expect_error(knapsack_greedy(x = xNotDf3, W = 1), 
               "x has to be of the type data.frame with two columns.")
  expect_error(knapsack_greedy(x = xWrongColumns1, W = 1), 
               "x has to be of the type data.frame with two columns.")
  expect_error(knapsack_greedy(x = xWrongColumns2, W = 1), 
               "x has to be of the type data.frame with two columns.")
})


# Outputs ----------------------------------------------------------------------
context("knapsack_greedy outputs")

# Test on small data
knapsack_objects <- data.frame(w = c(1, 3, 4, 5), v = c(1, 4, 5, 7))
test_that("Outputs", {
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 7)$value, 8)
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 7)$elements, c(4, 1))
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 5)$value, 7)
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 5)$elements, c(4))
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 13)$value, 17)
  expect_equal(knapsack_greedy(x = knapsack_objects, W = 13)$elements, c(4, 2, 3, 1))
})

knapsack_objects <- genKnapsack(2000)
test_that("Outputs", {
  expect_equal(knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)$value, 192647)
})
