test_that("one join key: global filter uses joined column and drops unmatched rows", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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

test_that("two join keys: unmatched rows on second key receive NA", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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
      self = "adlb",
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

test_that("domain filter: keeps rows per SRC_ rule and removes SRC_", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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

test_that("global filter: removes rows with NA or non-positive AVAL", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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

test_that("keep_vars: keeps only listed columns in listed order", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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

test_that("all blocks: join, domain filter, global filter and keep_vars combine", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      self = "adlb",
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
