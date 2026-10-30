test_that("copies source_var into a new rename_var column at the end and keeps source_var", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_mutate.mustache",
    params = list(
      domain = "adlb",
      rename_var = "AVAL",
      source_var = "LBSTRESN"
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "01", "02"),
    PARAMCD = c("ALT", "AST", "ALT"),
    LBSTRESN = c(1.5, NA, 3)
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01", "02"),
    PARAMCD = c("ALT", "AST", "ALT"),
    LBSTRESN = c(1.5, NA, 3),
    AVAL = c(1.5, NA, 3)
  )

  expect_equal(result, expected)
})

test_that("does not protect against an existing rename_var column: it is overwritten in place", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_mutate.mustache",
    params = list(
      domain = "adlb",
      rename_var = "AVAL",
      source_var = "LBSTRESN"
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "02"),
    AVAL = c(99, 99),
    LBSTRESN = c(1.5, NA)
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    AVAL = c(1.5, NA),
    LBSTRESN = c(1.5, NA)
  )

  expect_equal(result, expected)
})
