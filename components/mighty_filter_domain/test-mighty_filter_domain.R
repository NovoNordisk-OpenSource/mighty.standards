test_that("J1 one join: joined column is usable in global_filter and unmatched rows are dropped", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(list(
        table = "adsl",
        select_expr = "USUBJID, SEX",
        keys = "\"USUBJID\""
      )),
      domain_filter = NULL,
      global_filter = "SEX == 'F'",
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    USUBJID = c("01", "02", "03"),
    AVAL = c(1, 2, 3)
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
    USUBJID = "01",
    AVAL = 1,
    SEX = "F"
  )

  expect_equal(result, expected)
})

test_that("J2 two join keys: row matching on one key only receives NA", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(list(
        table = "adsl",
        select_expr = "STUDYID, USUBJID, SEX",
        keys = "\"STUDYID\", \"USUBJID\""
      )),
      domain_filter = NULL,
      global_filter = "is.na(SEX) | SEX == 'F'",
      keep_vars = NULL
    )
  )

  adlb <- data.frame(
    STUDYID = c("STUDY1", "STUDY2"),
    USUBJID = c("01", "01")
  )
  adsl <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = "01",
    SEX = "F"
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

test_that("J3 duplicate key in join dataset raises many-to-one error", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(list(
        table = "adsl",
        select_expr = "USUBJID, SEX",
        keys = "\"USUBJID\""
      )),
      domain_filter = NULL,
      global_filter = "SEX == 'F'",
      keep_vars = NULL
    ),
    check_coverage = FALSE
  )

  adlb <- data.frame(
    USUBJID = "01",
    AVAL = 1
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

test_that("D1 domain filter: keeps LB rows and XL rows with CHEMISTRY, removes SRC_", {
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
    USUBJID = c("01", "02", "03", "04"),
    LBCAT = c("HEMATOLOGY", "CHEMISTRY", "CHEMISTRY", "HEMATOLOGY"),
    SRC_ = c("LB", "LB", "XL", "XL")
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02", "03"),
    LBCAT = c("HEMATOLOGY", "CHEMISTRY", "CHEMISTRY")
  )

  expect_equal(result, expected)
})

test_that("G1 global filter: comma-separated expressions are combined and rows failing any are removed", {
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
    AVAL = c(1, NA, 0, 5)
  )
  comp$assign("adlb", adlb)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "04"),
    AVAL = c(1, 5)
  )

  expect_equal(result, expected)
})

test_that("K1 keep_vars: result has only the listed columns, in the listed order", {
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
    PARAM = c("Alanine", "Aspartate"),
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

test_that("C1 all blocks: joins run before filters, SRC_ is removed, keep_vars runs last", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(list(
        table = "adsl",
        select_expr = "STUDYID, USUBJID, SEX",
        keys = "\"STUDYID\", \"USUBJID\""
      )),
      domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')",
      global_filter = "SEX == 'F'",
      keep_vars = "STUDYID, USUBJID, LBCAT, AVAL"
    )
  )

  adlb <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02", "03", "04"),
    LBCAT = c("HEMATOLOGY", "CHEMISTRY", "HEMATOLOGY", "CHEMISTRY"),
    AVAL = c(1, 2, 3, 4),
    SRC_ = c("LB", "LB", "XL", "XL")
  )
  adsl <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02", "03", "04"),
    SEX = c("F", "M", "F", "F")
  )
  comp$assign("adlb", adlb)
  comp$assign("adsl", adsl)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("01", "04"),
    LBCAT = c("HEMATOLOGY", "CHEMISTRY"),
    AVAL = c(1, 4)
  )

  expect_equal(result, expected)
})
