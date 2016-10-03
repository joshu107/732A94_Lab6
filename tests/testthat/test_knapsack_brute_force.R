context("Test knapsack_brute_force")

# test_that("Inputs", {
# 
# })


# Test on small data
knapsack_objects <- data.frame(w = c(1, 3, 4, 5), v = c(1, 4, 5, 7))
test_that("Outputs", {
  expect_equal(knapsack_brute_force(x = knapsack_objects, W = 7)$value, 9)
  expect_equal(knapsack_brute_force(x = knapsack_objects, W = 7)$elements, c(2, 3))
})

# Test on large data
knapsack_objects <- genKnapsack(2000)
test_that("Outputs", {
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)$value, 
               16770)
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)$elements,
               c(5, 8))
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500)$value, 
               16770)
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500)$elements,
               c(5, 8))
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)$value, 
               15428)
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)$elements,
               c(3, 8))
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)$value, 
               15428)
  expect_equal(knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)$elements,
               c(3, 8))  
})

