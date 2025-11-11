test_that("study_count counts unique studies correctly", {
  test_data <- data.frame(
    unique_study_id = c(1, 1, 2, 3, 2, 1, 3),
    other_var = c("a", "b", "c", "d", "e", "f", "g")
  )

  result <- study_count(test_data)

  expect_s3_class(result, "data.frame")
  expect_true("N_unique" %in% names(result))
  expect_equal(result$N_unique, 3)
})

test_that("study_count works with tibbles", {
  test_data <- tibble::tibble(
    unique_study_id = c(10, 20, 30, 10, 20),
    value = c(100, 200, 300, 400, 500)
  )

  result <- study_count(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(result$N_unique, 3)
})

test_that("study_count handles single unique value", {
  test_data <- data.frame(
    unique_study_id = c(1, 1, 1, 1, 1)
  )

  result <- study_count(test_data)

  expect_equal(result$N_unique, 1)
})

test_that("study_count handles all unique values", {
  test_data <- data.frame(
    unique_study_id = c(1, 2, 3, 4, 5)
  )

  result <- study_count(test_data)

  expect_equal(result$N_unique, 5)
})

test_that("study_count works with custom counting variable", {
  test_data <- data.frame(
    unique_study_id = c(1, 1, 2, 2, 3),
    custom_id = c("A", "A", "B", "C", "C")
  )

  result <- study_count(test_data, counting_var = "custom_id")

  expect_equal(result$N_unique, 3)
})

test_that("study_count works with character IDs", {
  test_data <- data.frame(
    unique_study_id = c("study1", "study2", "study1", "study3", "study2", "study1")
  )

  result <- study_count(test_data)

  expect_equal(result$N_unique, 3)
})

test_that("study_count works with included dataset", {
  result <- study_count(PaluckMetaSOP::sv_data)

  expect_s3_class(result, "data.frame")
  expect_true("N_unique" %in% names(result))
  expect_true(result$N_unique > 0)
  expect_true(result$N_unique <= nrow(PaluckMetaSOP::sv_data))
})

test_that("study_count handles NA values correctly", {
  test_data <- data.frame(
    unique_study_id = c(1, 2, NA, 1, 3, NA)
  )

  result <- study_count(test_data)

  expect_s3_class(result, "data.frame")
  expect_true(result$N_unique >= 3)  # At least 1, 2, 3 (NA may or may not be counted)
})

test_that("study_count returns a data frame with correct structure", {
  test_data <- data.frame(
    unique_study_id = c(1, 2, 3, 1, 2)
  )

  result <- study_count(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
  expect_equal(nrow(result), 1)
  expect_equal(names(result), "N_unique")
})

test_that("study_count works with factor variables", {
  test_data <- data.frame(
    unique_study_id = factor(c("A", "B", "A", "C", "B", "A"))
  )

  result <- study_count(test_data)

  expect_equal(result$N_unique, 3)
})
