test_that("no rename: matches lookup data and keeps self row count", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = list(
      self = "adlb",
      join_dataset = "adsl",
      select_expr = "USUBJID, SEX",
      by_vars = "\"USUBJID\"",
      needs_rename = FALSE,
      output_var = "SEX",
      var_to_add = "SEX"
    )
  )

  comp$assign(
    "adlb",
    data.frame(
      USUBJID = c("01", "01", "02"),
      PARAM = c("ALT", "AST", "ALT")
    )
  )
  comp$assign(
    "adsl",
    data.frame(
      USUBJID = c("01", "03"),
      SEX = c("F", "M"),
      IGNORED_COL = c("X", "Y")
    )
  )

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01", "02"),
    PARAM = c("ALT", "AST", "ALT"),
    SEX = c("F", "F", NA_character_)
  )

  expect_equal(result, expected)
})

test_that("rename: output_var is added and var_to_add is absent in result", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = list(
      self = "adlb",
      join_dataset = "adsl",
      select_expr = "USUBJID, SEX",
      by_vars = "\"USUBJID\"",
      needs_rename = TRUE,
      output_var = "GENDER",
      var_to_add = "SEX"
    )
  )

  comp$assign(
    "adlb",
    data.frame(
      USUBJID = c("01", "02")
    )
  )
  comp$assign(
    "adsl",
    data.frame(
      USUBJID = c("01", "02"),
      SEX = c("F", "M")
    )
  )

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    GENDER = c("F", "M")
  )

  expect_equal(result, expected)
})

test_that("two join keys: unmatched rows on second key receive NA", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = list(
      self = "adlb",
      join_dataset = "adsl",
      select_expr = "STUDYID, USUBJID, SEX",
      by_vars = "\"STUDYID\", \"USUBJID\"",
      needs_rename = FALSE,
      output_var = "SEX",
      var_to_add = "SEX"
    )
  )

  comp$assign(
    "adlb",
    data.frame(
      STUDYID = c("STUDY1", "STUDY2"),
      USUBJID = c("01", "01")
    )
  )
  comp$assign(
    "adsl",
    data.frame(
      STUDYID = c("STUDY1"),
      USUBJID = c("01"),
      SEX = c("F")
    )
  )

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    STUDYID = c("STUDY1", "STUDY2"),
    USUBJID = c("01", "01"),
    SEX = c("F", NA_character_)
  )

  expect_equal(result, expected)
})

test_that("duplicate key in join_dataset raises many-to-one error", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = list(
      self = "adlb",
      join_dataset = "adsl",
      select_expr = "USUBJID, SEX",
      by_vars = "\"USUBJID\"",
      needs_rename = FALSE,
      output_var = "SEX",
      var_to_add = "SEX"
    ),
    check_coverage = FALSE
  )

  comp$assign(
    "adlb",
    data.frame(
      USUBJID = c("01")
    )
  )
  comp$assign(
    "adsl",
    data.frame(
      USUBJID = c("01", "01"),
      SEX = c("F", "M")
    )
  )

  expect_error(
    comp$eval(),
    class = "dplyr_error_join_relationship_many_to_one"
  )
})

test_that("rename when self already contains var_to_add keeps existing column", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_echo.mustache",
    params = list(
      self = "adlb",
      join_dataset = "adsl",
      select_expr = "USUBJID, SEX",
      by_vars = "\"USUBJID\"",
      needs_rename = TRUE,
      output_var = "GENDER",
      var_to_add = "SEX"
    )
  )

  comp$assign(
    "adlb",
    data.frame(
      USUBJID = c("01", "02"),
      SEX = c("ORIGINAL_1", "ORIGINAL_2")
    )
  )
  comp$assign(
    "adsl",
    data.frame(
      USUBJID = c("01", "02"),
      SEX = c("F", "M")
    )
  )

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    SEX = c("ORIGINAL_1", "ORIGINAL_2"),
    GENDER = c("F", "M")
  )

  expect_equal(result, expected)
})
