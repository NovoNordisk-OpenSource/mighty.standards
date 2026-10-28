test_that("one join: only select_expr columns are added, rows without a match get NA and fail global_filter", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "adsl",
          select_expr = c("USUBJID", "SEX"),
          keys = "USUBJID"
        )
      ),
      domain_filter = NULL,
      global_filter = "SEX == 'F'",
      has_keep_vars = FALSE,
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
          select_expr = c("STUDYID", "USUBJID", "SEX"),
          keys = c("STUDYID", "USUBJID")
        )
      ),
      domain_filter = NULL,
      global_filter = NULL,
      has_keep_vars = FALSE,
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

test_that("duplicate key in join dataset raises many-to-one error", {
  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "adsl",
          select_expr = c("USUBJID", "SEX"),
          keys = "USUBJID"
        )
      ),
      domain_filter = NULL,
      global_filter = NULL,
      has_keep_vars = FALSE,
      keep_vars = NULL
    ),
    check_coverage = FALSE
  )

  adlb <- data.frame(
    USUBJID = "01"
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
          select_expr = c("USUBJID", "SEX"),
          keys = "USUBJID"
        ),
        list(
          table = "ex",
          select_expr = c("USUBJID", "EXTRT"),
          keys = "USUBJID"
        )
      ),
      domain_filter = NULL,
      global_filter = "SEX == 'F', EXTRT == 'DRUG'",
      has_keep_vars = FALSE,
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
      has_keep_vars = FALSE,
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
      has_keep_vars = FALSE,
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
    USUBJID = "01",
    AVAL = 5
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
      has_keep_vars = TRUE,
      keep_vars = c("USUBJID", "PARAMCD", "AVAL")
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
          select_expr = c("STUDYID", "USUBJID", "SEX"),
          keys = c("STUDYID", "USUBJID")
        )
      ),
      domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')",
      global_filter = "SEX == 'F'",
      has_keep_vars = TRUE,
      keep_vars = c("USUBJID", "PARAMCD", "AVAL")
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

test_that("join column already in domain: global_filter cannot find the column because the join added .x/.y suffixes", {
  # Mirrors a spec where ADLB copies VISIT from LB and a global filter
  # depends on SV.VISIT; all params are as generated by mighty

  # The error message comes from base R, which translates it; force English
  withr::local_language("en")

  comp <- mighty.component::get_test_component(
    component = "mighty_filter_domain.mustache",
    params = list(
      domain = "adlb",
      joins = list(
        list(
          table = "sv",
          select_expr = c("VISIT", "STUDYID", "USUBJID", "VISITNUM"),
          keys = c("STUDYID", "USUBJID", "VISITNUM")
        )
      ),
      domain_filter = NULL,
      global_filter = "!grepl(\"UNSCHEDULED\", VISIT)",
      has_keep_vars = TRUE,
      keep_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD", "VISITNUM", "VISIT")
    ),
    check_coverage = FALSE
  )

  adlb <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("01", "01"),
    LBSEQ = c(1, 2),
    LBTESTCD = c("ALT", "ALT"),
    VISITNUM = c(1, 1.1),
    VISIT = c("BASELINE", "UNSCHEDULED 1.1")
  )
  sv <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("01", "01"),
    VISITNUM = c(1, 1.1),
    VISIT = c("BASELINE", "UNSCHEDULED 1.1")
  )
  comp$assign("adlb", adlb)
  comp$assign("sv", sv)

  expect_error(comp$eval(), "object 'VISIT' not found", fixed = TRUE)
})
