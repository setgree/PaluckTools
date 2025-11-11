test_that("d_calc handles stat_type 'd' correctly", {
  result <- d_calc(stat_type = "d", stat = 0.5, sample_sd = NA, n_t = NA, n_c = NA)
  expect_equal(result, 0.5)
})

test_that("d_calc handles stat_type 's_m_d' correctly", {
  result <- d_calc(stat_type = "s_m_d", stat = 0.3, sample_sd = NA, n_t = NA, n_c = NA)
  expect_equal(result, 0.3)
})

test_that("d_calc handles stat_type 'd_i_m' correctly", {
  result <- d_calc(stat_type = "d_i_m", stat = 1, sample_sd = 0.3, n_t = NA, n_c = NA)
  expect_equal(result, round(1 / 0.3, digits = 3))
})

test_that("d_calc handles stat_type 'd_i_d' correctly", {
  result <- d_calc(stat_type = "d_i_d", stat = 0.5, sample_sd = 0.25, n_t = NA, n_c = NA)
  expect_equal(result, round(0.5 / 0.25, digits = 3))
})

test_that("d_calc handles stat_type 'reg_coef' correctly", {
  result <- d_calc(stat_type = "reg_coef", stat = 0.6, sample_sd = 0.2, n_t = NA, n_c = NA)
  expect_equal(result, round(0.6 / 0.2, digits = 3))
})

test_that("d_calc handles stat_type 'regression' correctly", {
  result <- d_calc(stat_type = "regression", stat = 0.4, sample_sd = 0.1, n_t = NA, n_c = NA)
  expect_equal(result, round(0.4 / 0.1, digits = 3))
})

test_that("d_calc handles stat_type 'beta' correctly", {
  result <- d_calc(stat_type = "beta", stat = 0.3, sample_sd = 0.15, n_t = NA, n_c = NA)
  expect_equal(result, round(0.3 / 0.15, digits = 3))
})

test_that("d_calc handles stat_type 't_test' correctly", {
  result <- d_calc(stat_type = "t_test", stat = 2, sample_sd = NA, n_t = 50, n_c = 40)
  expected <- round(2 * sqrt((50 + 40) / (50 * 40)), digits = 3)
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'T-test' correctly", {
  result <- d_calc(stat_type = "T-test", stat = 1.5, sample_sd = NA, n_t = 30, n_c = 30)
  expected <- round(1.5 * sqrt((30 + 30) / (30 * 30)), digits = 3)
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'f_test' correctly", {
  result <- d_calc(stat_type = "f_test", stat = 4, sample_sd = NA, n_t = 50, n_c = 40)
  expected <- round(sqrt((4 * (50 + 40)) / (50 * 40)), digits = 3)
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'F-test' correctly", {
  result <- d_calc(stat_type = "F-test", stat = 3, sample_sd = NA, n_t = 25, n_c = 25)
  expected <- round(sqrt((3 * (25 + 25)) / (25 * 25)), digits = 3)
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'F' correctly", {
  result <- d_calc(stat_type = "F", stat = 2.5, sample_sd = NA, n_t = 40, n_c = 60)
  expected <- round(sqrt((2.5 * (40 + 60)) / (40 * 60)), digits = 3)
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'odds_ratio' correctly", {
  result <- d_calc(stat_type = "odds_ratio", stat = 2, sample_sd = NA, n_t = NA, n_c = NA)
  expected <- log(2) * sqrt(3) / pi
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'log_odds_ratio' correctly", {
  result <- d_calc(stat_type = "log_odds_ratio", stat = 0.5, sample_sd = NA, n_t = NA, n_c = NA)
  expected <- 0.5 * sqrt(3) / pi
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'd_i_p' correctly", {
  result <- d_calc(stat_type = "d_i_p", stat = 0.2, sample_sd = 0.5, n_t = NA, n_c = NA)
  expected <- 0.2 / sqrt(0.5 * (1 - 0.5))
  expect_equal(result, expected)
})

test_that("d_calc handles stat_type 'unspecified_null' correctly", {
  result <- d_calc(stat_type = "unspecified_null", stat = NA, sample_sd = NA, n_t = NA, n_c = NA)
  expect_equal(result, 0.01)
})

test_that("d_calc handles stat_type 'unspecified null' correctly", {
  result <- d_calc(stat_type = "unspecified null", stat = NA, sample_sd = NA, n_t = NA, n_c = NA)
  expect_equal(result, 0.01)
})

test_that("d_calc returns NA for unrecognized stat_type", {
  result <- d_calc(stat_type = "invalid_type", stat = 1, sample_sd = 0.3, n_t = 50, n_c = 40)
  expect_true(is.na(result))
})

test_that("d_calc example from documentation works", {
  result <- d_calc(stat_type = "d_i_m", stat = 1, sample_sd = 0.3)
  expect_equal(result, round(1 / 0.3, digits = 3))

  result2 <- d_calc(stat_type = "f_test", stat = 1, n_t = 50, n_c = 40)
  expected2 <- round(sqrt((1 * (50 + 40)) / (50 * 40)), digits = 3)
  expect_equal(result2, expected2)
})
