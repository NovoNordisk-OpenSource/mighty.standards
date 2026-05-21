# Parameter contracts for the _init_domain component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_single_source <- list(
  self = "ADSL",
  keep_vars = "USUBJID, AGE, SEX",
  source_domain_rbind = "DM",
  src_mutations = list()
)

params_multi_source_with_src <- list(
  self = "ADLB",
  keep_vars = "USUBJID, PARAMCD, AVAL, SRC_",
  source_domain_rbind = "rbind(LB,\nXL)",
  src_mutations = list(
    list(domain = "LB"),
    list(domain = "XL")
  )
)

# mocks ------------------------------------------------------------------------
mock_dm <- data.frame(USUBJID = "U001", AGE = 30L, SEX = "M", stringsAsFactors = FALSE)
mock_lb <- data.frame(USUBJID = "U001", PARAMCD = "ALT", AVAL = 1.0, stringsAsFactors = FALSE)
mock_xl <- data.frame(USUBJID = "U001", PARAMCD = "XLT", AVAL = 2.0, stringsAsFactors = FALSE)

# tests ------------------------------------------------------------------------

test_that("single source domain: renders select and convert_blanks_to_na without src mutations", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_init_domain.mustache",
    params = params_single_source
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADSL <-  DM |>", fixed = TRUE)
  expect_match(rendered, "dplyr::select(USUBJID, AGE, SEX)", fixed = TRUE)
  expect_match(rendered, "admiral::convert_blanks_to_na()", fixed = TRUE)
  expect_no_match(rendered, "dplyr::mutate(SRC_", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "DM", value = mock_dm)
  component$eval()
})

test_that("multiple source domains with SRC_: renders per-domain mutate and rbind", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_init_domain.mustache",
    params = params_multi_source_with_src
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, 'LB <- LB |>\n  dplyr::mutate(SRC_ = "LB")', fixed = TRUE)
  expect_match(rendered, 'XL <- XL |>\n  dplyr::mutate(SRC_ = "XL")', fixed = TRUE)
  expect_match(rendered, "ADLB <-  rbind(LB,\nXL) |>", fixed = TRUE)
  expect_match(rendered, "dplyr::select(USUBJID, PARAMCD, AVAL, SRC_)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "LB", value = mock_lb)
  component$assign(x = "XL", value = mock_xl)
  component$eval()
})

