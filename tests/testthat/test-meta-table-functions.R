test_that("extract_model_results works with sv_data", {
  skip_if_not_installed("metafor")
  result <- sv_data |> extract_model_results()

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 8)
  expect_equal(nrow(result), 1)
  expect_true("Approach" %in% names(result))
  expect_true("Delta" %in% names(result))
  expect_equal(result$Approach, "Overall")
})

test_that("extract_model_results works with contact_data", {
  result <- contact_data |> extract_model_results(approach_name = "Contact interventions")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$Approach, "Contact interventions")
  expect_true(is.numeric(result$Delta))
})

test_that("run_subset_meta_analysis works", {
  result <- sv_data |>
    run_subset_meta_analysis(
      group_var = "study_design",
      level = "Randomized Controlled Trial",
      include_tau = TRUE
    )

  expect_s3_class(result, "tbl_df")
  expect_true("tau" %in% names(result))
  expect_equal(result$Moderator, "Randomized Controlled Trial")
})

test_that("process_group works", {
  result <- sv_data |>
    process_group(
      group_var = "study_design",
      ref_level = "Randomized Controlled Trial"
    )

  expect_s3_class(result, "tbl_df")
  expect_true("p_val_ref" %in% names(result))
  expect_true("ref" %in% result$p_val_ref)
  expect_true(nrow(result) > 1)
})

test_that("p-values are formatted correctly for small values", {
  result <- sv_data |> extract_model_results()

  # Should format very small p-values as "< 0.001"
  expect_true(result$p_val == "< 0.001" | grepl("^\\.", result$p_val))
})
