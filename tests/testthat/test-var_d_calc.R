test_that("var_d_calc calculates variance correctly for basic inputs", {
  # Test with d = 0.5, n_t = 50, n_c = 50
  d <- 0.5
  n_t <- 50
  n_c <- 50

  # Manual calculation
  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))
  expected <- round((hedge_g^2) * ust_var_d, digits = 3)

  result <- var_d_calc(d = d, n_t = n_t, n_c = n_c)
  expect_equal(result, expected)
})

test_that("var_d_calc calculates variance correctly with unequal sample sizes", {
  d <- 0.8
  n_t <- 30
  n_c <- 45

  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))
  expected <- round((hedge_g^2) * ust_var_d, digits = 3)

  result <- var_d_calc(d = d, n_t = n_t, n_c = n_c)
  expect_equal(result, expected)
})

test_that("var_d_calc handles small effect size", {
  d <- 0.1
  n_t <- 100
  n_c <- 100

  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))
  expected <- round((hedge_g^2) * ust_var_d, digits = 3)

  result <- var_d_calc(d = d, n_t = n_t, n_c = n_c)
  expect_equal(result, expected)
})

test_that("var_d_calc handles large effect size", {
  d <- 2.0
  n_t <- 20
  n_c <- 20

  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))
  expected <- round((hedge_g^2) * ust_var_d, digits = 3)

  result <- var_d_calc(d = d, n_t = n_t, n_c = n_c)
  expect_equal(result, expected)
})

test_that("var_d_calc handles zero effect size", {
  d <- 0
  n_t <- 50
  n_c <- 50

  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))
  expected <- round((hedge_g^2) * ust_var_d, digits = 3)

  result <- var_d_calc(d = d, n_t = n_t, n_c = n_c)
  expect_equal(result, expected)
})

test_that("var_d_calc example from documentation works", {
  # Example from documentation
  ditullio_results <- d_calc(stat_type = "d_i_m", stat = 5.708 - 3.0798, sample_sd = 1.0381)
  ditullio_variance <- var_d_calc(d = ditullio_results, n_t = 38, n_c = 38)

  # Check that variance is calculated and is positive
  expect_type(ditullio_variance, "double")
  expect_true(ditullio_variance > 0)

  # Verify SE can be calculated
  ditullio_se <- sqrt(ditullio_variance)
  expect_true(ditullio_se > 0)
})

test_that("var_d_calc returns numeric result", {
  result <- var_d_calc(d = 0.5, n_t = 30, n_c = 30)
  expect_type(result, "double")
})

test_that("var_d_calc variance increases with smaller sample sizes", {
  d <- 0.5
  var_large_n <- var_d_calc(d = d, n_t = 100, n_c = 100)
  var_small_n <- var_d_calc(d = d, n_t = 20, n_c = 20)

  expect_true(var_small_n > var_large_n)
})
