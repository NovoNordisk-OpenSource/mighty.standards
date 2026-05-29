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

# data -------------------------------------------------------------------------
test_subjects <- c("01-701-1015", "01-701-1023", "01-701-1028")

adlb <- pharmaversesdtm::lb[
  pharmaversesdtm::lb$LBTESTCD == "ALT" &
    pharmaversesdtm::lb$USUBJID %in% test_subjects,
  c("STUDYID", "USUBJID", "LBTESTCD", "LBSTRESN")
]

adsl <- pharmaversesdtm::dm[
  pharmaversesdtm::dm$USUBJID %in% test_subjects,
  c("STUDYID", "USUBJID", "SEX", "ARMCD", "COUNTRY")
]

# tests ------------------------------------------------------------------------

test_that("no rename: renders left_join without trailing rename call", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = params_no_rename
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::rename", fixed = TRUE)
  component$assign(x = "ADLB", value = adlb)
  component$assign(x = "ADSL", value = adsl[, c("USUBJID", "SEX")])
  result <- component$eval()$get("ADLB")
  expect_true("SEX" %in% names(result))
})

test_that("with rename: renders left_join followed by rename call", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = params_with_rename
  )

  # EXPECT ---------------------------------------------------------------------
  component$assign(x = "ADLB", value = adlb)
  component$assign(x = "ADSL", value = adsl[, c("USUBJID", "ARMCD")])
  result <- component$eval()$get("ADLB")
  expect_true("TRTP" %in% names(result))
  expect_false("ARMCD" %in% names(result))
})

test_that("multi-key join: renders join with composite by_vars", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = params_multi_key
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::rename", fixed = TRUE)
  component$assign(x = "ADLB", value = adlb)
  component$assign(x = "ADSL", value = adsl[, c("STUDYID", "USUBJID", "COUNTRY")])
  result <- component$eval()$get("ADLB")
  expect_true("COUNTRY" %in% names(result))
})
