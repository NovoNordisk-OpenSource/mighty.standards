# Parameter contracts for the _col_echo component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
# Column name matches in both datasets — no rename needed
params_no_rename <- list(
  self = "ADLB",
  join_dataset = "ADSL",
  select_expr = "USUBJID, SEX",
  by_vars = '"USUBJID"',
  needs_rename = FALSE,
  output_var = "SEX",
  var_to_add = "SEX"
)

# Column has a different name in the source dataset — rename required
params_with_rename <- list(
  self = "ADLB",
  join_dataset = "ADSL",
  select_expr = "USUBJID, ARMCD",
  by_vars = '"USUBJID"',
  needs_rename = TRUE,
  output_var = "TRTP",
  var_to_add = "ARMCD"
)

# Multiple join keys
params_multi_key <- list(
  self = "ADLB",
  join_dataset = "ADSL",
  select_expr = "STUDYID, USUBJID, COUNTRY",
  by_vars = '"STUDYID", "USUBJID"',
  needs_rename = FALSE,
  output_var = "COUNTRY",
  var_to_add = "COUNTRY"
)

# mocks ------------------------------------------------------------------------
mock_adlb <- data.frame(
  USUBJID = "U001",
  PARAMCD = "ALT",
  stringsAsFactors = FALSE
)
mock_adlb_multi <- data.frame(
  STUDYID = "S001",
  USUBJID = "U001",
  PARAMCD = "ALT",
  stringsAsFactors = FALSE
)
mock_adsl <- data.frame(
  USUBJID = "U001",
  SEX = "M",
  ARMCD = "A",
  COUNTRY = "US",
  stringsAsFactors = FALSE
)
mock_adsl_multi <- data.frame(
  STUDYID = "S001",
  USUBJID = "U001",
  COUNTRY = "US",
  stringsAsFactors = FALSE
)

# tests ------------------------------------------------------------------------

test_that("no rename: renders left_join without trailing rename call", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_echo.mustache",
    params = params_no_rename
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADLB <- ADLB |>", fixed = TRUE)
  expect_match(
    rendered,
    "dplyr::left_join(ADSL |> dplyr::select(USUBJID, SEX),",
    fixed = TRUE
  )
  expect_match(rendered, 'by = c("USUBJID"))', fixed = TRUE)
  expect_no_match(rendered, "dplyr::rename", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb)
  component$assign(x = "ADSL", value = mock_adsl[, c("USUBJID", "SEX")])
  result <- component$eval()$get("ADLB")
  expect_true("SEX" %in% names(result))
})

test_that("with rename: renders left_join followed by rename call", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_echo.mustache",
    params = params_with_rename
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "dplyr::left_join(ADSL |> dplyr::select(USUBJID, ARMCD),",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::rename(TRTP = ARMCD)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb)
  component$assign(x = "ADSL", value = mock_adsl[, c("USUBJID", "ARMCD")])
  result <- component$eval()$get("ADLB")
  expect_true("TRTP" %in% names(result))
  expect_false("ARMCD" %in% names(result))
})

test_that("multi-key join: renders join with composite by_vars", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_echo.mustache",
    params = params_multi_key
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "dplyr::left_join(ADSL |> dplyr::select(STUDYID, USUBJID, COUNTRY),",
    fixed = TRUE
  )
  expect_match(rendered, 'by = c("STUDYID", "USUBJID"))', fixed = TRUE)
  expect_no_match(rendered, "dplyr::rename", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb_multi)
  component$assign(x = "ADSL", value = mock_adsl_multi)
  result <- component$eval()$get("ADLB")
  expect_true("COUNTRY" %in% names(result))
})
