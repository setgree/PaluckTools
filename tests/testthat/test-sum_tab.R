test_that("summarise_table creates frequency table for simple data", {
  test_data <- data.frame(
    category = c("A", "B", "A", "C", "B", "A")
  )

  result <- summarise_table(test_data, category)

  expect_s3_class(result, "table")
  expect_equal(as.numeric(result["A"]), 3)
  expect_equal(as.numeric(result["B"]), 2)
  expect_equal(as.numeric(result["C"]), 1)
})

test_that("summarise_table works with factor variables", {
  test_data <- data.frame(
    status = factor(c("active", "inactive", "active", "active", "inactive"))
  )

  result <- summarise_table(test_data, status)

  expect_s3_class(result, "table")
  expect_equal(as.numeric(result["active"]), 3)
  expect_equal(as.numeric(result["inactive"]), 2)
})

test_that("summarise_table works with numeric variables", {
  test_data <- data.frame(
    value = c(1, 2, 1, 3, 2, 1, 2)
  )

  result <- summarise_table(test_data, value)

  expect_s3_class(result, "table")
  expect_equal(as.numeric(result["1"]), 3)
  expect_equal(as.numeric(result["2"]), 3)
  expect_equal(as.numeric(result["3"]), 1)
})

test_that("summarise_table handles single unique value", {
  test_data <- data.frame(
    same = c("X", "X", "X", "X")
  )

  result <- summarise_table(test_data, same)

  expect_s3_class(result, "table")
  expect_equal(length(result), 1)
  expect_equal(as.numeric(result["X"]), 4)
})

test_that("summarise_table handles all unique values", {
  test_data <- data.frame(
    unique_vals = c("A", "B", "C", "D")
  )

  result <- summarise_table(test_data, unique_vals)

  expect_s3_class(result, "table")
  expect_equal(length(result), 4)
  expect_true(all(result == 1))
})

test_that("summarise_table works with tibbles", {
  test_data <- tibble::tibble(
    category = c("X", "Y", "X", "Z", "Y", "X")
  )

  result <- summarise_table(test_data, category)

  expect_s3_class(result, "table")
  expect_equal(as.numeric(result["X"]), 3)
  expect_equal(as.numeric(result["Y"]), 2)
  expect_equal(as.numeric(result["Z"]), 1)
})

test_that("summarise_table works with included dataset", {
  # Use the sv_data from the package
  result <- summarise_table(BLPlabtools::sv_data, behavior_type)

  expect_s3_class(result, "table")
  expect_true(length(result) > 0)
  expect_true(sum(result) == nrow(BLPlabtools::sv_data))
})

test_that("summarise_table handles NA values correctly", {
  test_data <- data.frame(
    category = c("A", "B", NA, "A", "B", NA)
  )

  result <- summarise_table(test_data, category)

  expect_s3_class(result, "table")
  # table() includes NA as a separate category
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
})
