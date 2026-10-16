# Parameter contracts for the _filter_domain component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_no_filters <- list(
  self = "ADLB",
  joins = list(),
  domain_filter = NULL,
  global_filter = NULL,
  keep_vars = "USUBJID, PARAMCD, AVAL"
)

params_domain_filter_only <- list(
  self = "ADLB",
  joins = list(),
  domain_filter = "(SRC_ == 'LB')",
  global_filter = NULL,
  keep_vars = "USUBJID, PARAMCD, AVAL"
)

params_global_filter_only <- list(
  self = "ADLB",
  joins = list(),
  domain_filter = NULL,
  global_filter = "!is.na(AVAL)",
  keep_vars = "USUBJID, PARAMCD, AVAL"
)

params_with_join_and_filters <- list(
  self = "ADLB",
  joins = list(
    list(
      table = "ADSL",
      select_expr = "USUBJID, SEX",
      keys = '"USUBJID"'
    )
  ),
  domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL')",
  global_filter = "SEX == 'M'",
  keep_vars = "USUBJID, PARAMCD, AVAL"
)

params_no_keep_vars <- list(
  self = "ADSL",
  joins = list(),
  domain_filter = NULL,
  global_filter = "!is.na(USUBJID)",
  keep_vars = NULL
)

# mocks ------------------------------------------------------------------------
mock_adlb <- data.frame(
  USUBJID = c("U001", "U002"),
  PARAMCD = c("ALT", "XLT"),
  AVAL = c(1.0, NA_real_),
  SRC_ = c("LB", "XL"),
  stringsAsFactors = FALSE
)
mock_adsl <- data.frame(
  USUBJID = c("U001", "U002"),
  SEX = c("M", "F"),
  stringsAsFactors = FALSE
)
mock_adsl_no_filter <- data.frame(
  USUBJID = "U001",
  stringsAsFactors = FALSE
)

# tests ------------------------------------------------------------------------

test_that("no filters: renders only keep_vars select block", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_filter_domain.mustache",
    params = params_no_filters
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADLB <-  ADLB |>\n  dplyr::select(USUBJID, PARAMCD, AVAL)", fixed = TRUE)
  expect_no_match(rendered, "dplyr::filter", fixed = TRUE)
  expect_no_match(rendered, "dplyr::left_join", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb[, c("USUBJID", "PARAMCD", "AVAL", "SRC_")])
  component$eval()
})

test_that("domain filter: renders filter on SRC_ and select(-SRC_)", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_filter_domain.mustache",
    params = params_domain_filter_only
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "dplyr::filter((SRC_ == 'LB'))", fixed = TRUE)
  expect_match(rendered, "dplyr::select(-SRC_)", fixed = TRUE)
  expect_no_match(rendered, "dplyr::left_join", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb)
  component$eval()
})

test_that("global filter: renders filter block without touching SRC_", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_filter_domain.mustache",
    params = params_global_filter_only
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "dplyr::filter(!is.na(AVAL))", fixed = TRUE)
  expect_no_match(rendered, "dplyr::select(-SRC_)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb[, c("USUBJID", "PARAMCD", "AVAL", "SRC_")])
  component$eval()
})

test_that("join + domain filter + global filter: renders all three blocks in order", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_filter_domain.mustache",
    params = params_with_join_and_filters
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "dplyr::left_join(ADSL |> dplyr::select(USUBJID, SEX)", fixed = TRUE)
  expect_match(rendered, 'by = c("USUBJID"))', fixed = TRUE)
  expect_match(rendered, "dplyr::filter((SRC_ == 'LB') | (SRC_ == 'XL'))", fixed = TRUE)
  expect_match(rendered, "dplyr::select(-SRC_)", fixed = TRUE)
  expect_match(rendered, "dplyr::filter(SEX == 'M')", fixed = TRUE)
  # join block appears before filter block
  expect_gt(
    regexpr("dplyr::filter", rendered, fixed = TRUE)[[1]],
    regexpr("dplyr::left_join", rendered, fixed = TRUE)[[1]]
  )

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = mock_adlb)
  component$assign(x = "ADSL", value = mock_adsl)
  component$eval()
})

test_that("NULL keep_vars: omits the final select block", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_filter_domain.mustache",
    params = params_no_keep_vars
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "dplyr::filter(!is.na(USUBJID))", fixed = TRUE)
  expect_no_match(rendered, "dplyr::select(", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADSL", value = mock_adsl_no_filter)
  component$eval()
})
