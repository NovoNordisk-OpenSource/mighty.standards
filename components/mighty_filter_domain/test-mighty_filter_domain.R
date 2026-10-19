test_that("one join: joined column is usable in global_filter, unmatched rows are dropped, several rows per key are kept", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(table = "adsl", select_expr = "USUBJID, SEX", keys = "\"USUBJID\"")
      ),
      domain_filter = NULL,
      global_filter = "SEX == 'F'",
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "01", "02", "03"),
    PARAMCD = c("ALT", "AST", "ALT", "ALT")
  )
  adsl <- data.frame(
    USUBJID = c("01", "02"),
    SEX = c("F", "M"),
    IGNORED_COL = c("X", "Y")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01"),
    PARAMCD = c("ALT", "AST"),
    SEX = c("F", "F")
  )

  expect_equal(result, expected)
})

test_that("two join keys: row matching on one key only receives NA", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "adsl",
          select_expr = "STUDYID, USUBJID, SEX",
          keys = "\"STUDYID\", \"USUBJID\""
        )
      ),
      domain_filter = NULL,
      global_filter = NULL,
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    STUDYID = c("STUDY1", "STUDY2"),
    USUBJID = c("01", "01")
  )
  adsl <- data.frame(
    STUDYID = c("STUDY1"),
    USUBJID = c("01"),
    SEX = c("F")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    STUDYID = c("STUDY1", "STUDY2"),
    USUBJID = c("01", "01"),
    SEX = c("F", NA_character_)
  )

  expect_equal(result, expected)
})

test_that("duplicate key in join dataset raises many-to-one error", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(table = "adsl", select_expr = "USUBJID, SEX", keys = "\"USUBJID\"")
      ),
      domain_filter = NULL,
      global_filter = NULL,
      keep_vars = NULL
    ),
    check_coverage = FALSE
  )

  adlb <- data.frame(
    USUBJID = c("01")
  )
  adsl <- data.frame(
    USUBJID = c("01", "01"),
    SEX = c("F", "M")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)

  expect_error(
    comp$eval(),
    class = "dplyr_error_join_relationship_many_to_one"
  )
})

test_that("two joins: columns from both join datasets are usable in global_filter", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "adsl",
          select_expr = "USUBJID, SEX",
          keys = "\"USUBJID\""
        ),
        list(table = "ex", select_expr = "USUBJID, EXTRT", keys = "\"USUBJID\"")
      ),
      domain_filter = NULL,
      global_filter = "SEX == 'F', EXTRT == 'DRUG'",
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    AVAL = c(1, 2, 3, 4)
  )
  adsl <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    SEX = c("F", "F", "M", "F")
  )
  ex <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    EXTRT = c("DRUG", "PLACEBO", "DRUG", "PLACEBO")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)
  comp$assign("ex", ex)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = "01",
    AVAL = 1,
    SEX = "F",
    EXTRT = "DRUG"
  )

  expect_equal(result, expected)
})

test_that("domain filter: keeps LB rows and XL rows with CHEMISTRY, removes SRC_", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(),
      domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')",
      global_filter = NULL,
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "01", "01", "01"),
    SRC_ = c("LB", "LB", "XL", "XL"),
    LBCAT = c("CHEMISTRY", "HEMATOLOGY", "CHEMISTRY", "HEMATOLOGY"),
    AVAL = c(1, 2, 3, 4)
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01", "01"),
    LBCAT = c("CHEMISTRY", "HEMATOLOGY", "CHEMISTRY"),
    AVAL = c(1, 2, 3)
  )

  expect_equal(result, expected)
})

test_that("global filter: comma-separated expressions are combined and rows failing any are removed", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(),
      domain_filter = NULL,
      global_filter = "!is.na(AVAL), AVAL > 0",
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    AVAL = c(5, NA, 0, -1)
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01"),
    AVAL = c(5)
  )

  expect_equal(result, expected)
})

test_that("keep_vars: result has only the listed columns, in the listed order", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(),
      domain_filter = NULL,
      global_filter = NULL,
      keep_vars = "USUBJID, PARAMCD, AVAL"
    )
  )

  adlb <- data.frame(
    AVAL = c(1, 2),
    STUDYID = c("STUDY1", "STUDY1"),
    PARAMCD = c("ALT", "AST"),
    USUBJID = c("01", "02")
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    PARAMCD = c("ALT", "AST"),
    AVAL = c(1, 2)
  )

  expect_equal(result, expected)
})

test_that("all blocks: join, domain filter and global filter all apply, then keep_vars selects the final columns", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "adsl",
          select_expr = "STUDYID, USUBJID, SEX",
          keys = "\"STUDYID\", \"USUBJID\""
        )
      ),
      domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')",
      global_filter = "SEX == 'F'",
      keep_vars = "USUBJID, PARAMCD, AVAL"
    )
  )

  adlb <- data.frame(
    STUDYID = c("STUDY1", "STUDY1", "STUDY1", "STUDY1"),
    USUBJID = c("01", "02", "01", "01"),
    SRC_ = c("LB", "LB", "XL", "XL"),
    LBCAT = c("CHEMISTRY", "CHEMISTRY", "HEMATOLOGY", "CHEMISTRY"),
    PARAMCD = c("ALT", "ALT", "HGB", "BILI"),
    AVAL = c(1, 2, 3, 4)
  )
  # Row 1: LB, female -> kept
  # Row 2: LB, male -> dropped by global filter
  # Row 3: XL, not CHEMISTRY, female -> dropped by domain filter
  # Row 4: XL, CHEMISTRY, female -> kept
  adsl <- data.frame(
    STUDYID = c("STUDY1", "STUDY1"),
    USUBJID = c("01", "02"),
    SEX = c("F", "M")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "01"),
    PARAMCD = c("ALT", "BILI"),
    AVAL = c(1, 4)
  )

  expect_equal(result, expected)
})
