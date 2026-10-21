test_that("one source: result has only keep_vars columns, in keep_vars order", {
  comp <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = list(
      domain = "adlb",
      keep_vars = c("USUBJID", "PARAMCD", "AVAL"),
      source_domain_rbind = "LB",
      src_mutations = list()
    )
  )

  LB <- data.frame(
    AVAL = c(1, 2),
    STUDYID = c("STUDY1", "STUDY1"),
    PARAMCD = c("ALT", "AST"),
    USUBJID = c("01", "02")
  )
  comp$assign("LB", LB)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    PARAMCD = c("ALT", "AST"),
    AVAL = c(1, 2)
  )

  expect_equal(result, expected)
})

test_that("two sources: rows stacked in source order, SRC_ holds each source domain name for filter_domain", {
  # mighty_filter_domain filters on SRC_ == '<source domain name>' and then drops SRC_
  comp <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = list(
      domain = "adlb",
      keep_vars = c("USUBJID", "LBCAT", "SRC_"),
      source_domain_rbind = "rbind(LB,\nXL)",
      src_mutations = list(
        list(source_domain = "LB"),
        list(source_domain = "XL")
      )
    )
  )

  LB <- data.frame(
    USUBJID = c("01", "02"),
    LBCAT = c("CHEMISTRY", "HEMATOLOGY")
  )
  XL <- data.frame(
    USUBJID = "01",
    LBCAT = "CHEMISTRY"
  )
  comp$assign("LB", LB)
  comp$assign("XL", XL)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02", "01"),
    LBCAT = c("CHEMISTRY", "HEMATOLOGY", "CHEMISTRY"),
    SRC_ = c("LB", "LB", "XL")
  )

  expect_equal(result, expected)
})

test_that("one source with SRC_: every row holds the source domain name for filter_domain", {
  # mighty_filter_domain filters on SRC_ == '<source domain name>' and then drops SRC_
  comp <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = list(
      domain = "adlb",
      keep_vars = c("USUBJID", "SRC_"),
      source_domain_rbind = "LB",
      src_mutations = list(
        list(source_domain = "LB")
      )
    )
  )

  LB <- data.frame(
    USUBJID = c("01", "02")
  )
  comp$assign("LB", LB)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    SRC_ = c("LB", "LB")
  )

  expect_equal(result, expected)
})

test_that("blanks: empty strings in character columns become NA, numeric columns are unchanged", {
  comp <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = list(
      domain = "adlb",
      keep_vars = c("USUBJID", "LBSTRESC", "LBSTRESN"),
      source_domain_rbind = "LB",
      src_mutations = list()
    )
  )

  LB <- data.frame(
    USUBJID = c("01", "02"),
    LBSTRESC = c("1.5", ""),
    LBSTRESN = c(1.5, NA)
  )
  comp$assign("LB", LB)

  comp$eval()

  result <- comp$get("adlb")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    LBSTRESC = c("1.5", NA_character_),
    LBSTRESN = c(1.5, NA)
  )

  expect_equal(result, expected)
})

test_that("two sources, one missing a column: errors on rbind", {
  # records base rbind behavior if mighty ever passes sources whose columns do not match
  comp <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = list(
      domain = "adlb",
      keep_vars = c("USUBJID", "LBCAT"),
      source_domain_rbind = "rbind(LB,\nXL)",
      src_mutations = list()
    ),
    check_coverage = FALSE
  )

  LB <- data.frame(
    USUBJID = "01",
    LBCAT = "CHEMISTRY"
  )
  XL <- data.frame(
    USUBJID = "01"
  )
  comp$assign("LB", LB)
  comp$assign("XL", XL)

  expect_error(
    comp$eval(),
    regexp = "numbers of columns of arguments do not match"
  )
})
