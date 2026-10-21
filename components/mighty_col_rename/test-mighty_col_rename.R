test_that("replaces source_var with rename_var in place and drops source_var", {
  comp <- mighty.component::get_test_component(
    component = "mighty_col_rename.mustache",
    params = list(
      domain = "adlb",
      rename_var = "SRCSEQ",
      source_var = "LBSEQ"
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "01", "02"),
    LBSEQ = c(1L, 2L, NA),
    PARAMCD = c("ALT", "AST", "ALT")
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01", "02"),
    SRCSEQ = c(1L, 2L, NA),
    PARAMCD = c("ALT", "AST", "ALT")
  )

  expect_equal(result, expected)
})

test_that("existing rename_var column: errors on duplicate names", {
  # mighty does not select rename_var into the domain before this component runs;
  # this test records the template's behavior if it ever does
  comp <- mighty.component::get_test_component(
    component = "mighty_col_rename.mustache",
    params = list(
      domain = "adlb",
      rename_var = "SRCSEQ",
      source_var = "LBSEQ"
    ),
    check_coverage = FALSE
  )

  adlb <- data.frame(
    USUBJID = c("01", "02"),
    SRCSEQ = c(99L, 99L),
    LBSEQ = c(1L, 2L)
  )
  comp$assign("adlb", adlb)

  expect_error(
    comp$eval(),
    class = "vctrs_error_names_must_be_unique"
  )
})
