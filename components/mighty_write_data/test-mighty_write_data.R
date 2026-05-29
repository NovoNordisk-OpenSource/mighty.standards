# Parameter contracts for the _write_data component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_with_row_order_and_keep <- list(
  self = "ADSL",
  file_ext = "parquet",
  row_order_vars = "USUBJID",
  keep_vars = "USUBJID,\nAGE,\nSEX"
)

params_row_order_only <- list(
  self = "ADLB",
  file_ext = "parquet",
  row_order_vars = "USUBJID,\nPARAMCD",
  keep_vars = NULL
)

params_keep_only <- list(
  self = "ADSL",
  file_ext = "parquet",
  row_order_vars = NULL,
  keep_vars = "USUBJID,\nAGE"
)

params_write_only <- list(
  self = "ADSL",
  file_ext = "sas7bdat",
  row_order_vars = NULL,
  keep_vars = NULL
)

# mocks ------------------------------------------------------------------------
# _write_data uses `cnt` directly (not via connector::connect), so it is
# injected into the callr session using component$assign(). This avoids the
# need for any .test_fn string patching and does not require the connector
# package to be installed.
mock_cnt <- list(
  adam = list(
    write_cnt = function(...) invisible(NULL)
  )
)

# tests ------------------------------------------------------------------------

test_that("row order + keep vars: arranges rows, selects columns, and writes", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = params_with_row_order_and_keep
  )

  # EXPECT ---------------------------------------------------------------------
  component$assign(
    x = "ADSL",
    value = data.frame(
      SEX = "M",
      COUNTRY = "US",
      USUBJID = "U001",
      AGE = 30L,
      stringsAsFactors = FALSE
    )
  )
  component$assign(x = "cnt", value = mock_cnt)
  component$eval()
  expect_false("COUNTRY" %in% names(component$get("ADSL")))
  expect_equal(names(component$get("ADSL")), c("USUBJID", "AGE", "SEX"))
})

test_that("row order only: arranges rows and writes without selecting columns", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = params_row_order_only
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::select", fixed = TRUE)
  component$assign(
    x = "ADLB",
    value = data.frame(
      USUBJID = c("U001", "U001"),
      PARAMCD = c("BILI", "ALT"),
      AVAL = c(1.0, 2.0),
      stringsAsFactors = FALSE
    )
  )
  component$assign(x = "cnt", value = mock_cnt)
  result_adlb <- component$eval()$get("ADLB")
  expect_equal(result_adlb$PARAMCD, c("ALT", "BILI"))
  expect_equal(result_adlb$AVAL, c(2.0, 1.0))
})

test_that("keep vars only: selects columns and writes without sorting rows", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = params_keep_only
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::arrange", fixed = TRUE)
  component$assign(
    x = "ADSL",
    value = data.frame(
      USUBJID = "U001",
      AGE = 30L,
      SEX = "M",
      stringsAsFactors = FALSE
    )
  )
  component$assign(x = "cnt", value = mock_cnt)
  component$eval()
  expect_equal(names(component$get("ADSL")), c("USUBJID", "AGE"))
})

test_that("write only: writes with custom file extension and no sort or select", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = params_write_only
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::arrange", fixed = TRUE)
  expect_no_match(rendered, "dplyr::select", fixed = TRUE)
  component$assign(
    x = "ADSL",
    value = data.frame(USUBJID = "U001", stringsAsFactors = FALSE)
  )
  component$assign(x = "cnt", value = mock_cnt)
  component$eval()
  expect_equal(names(component$get("ADSL")), "USUBJID")
})
