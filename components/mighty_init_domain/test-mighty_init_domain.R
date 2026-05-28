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

test_that("single source domain: select and convert_blanks_to_na without src mutations", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = params_single_source
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::mutate(SRC_", fixed = TRUE)
  component$assign(x = "DM", value = mock_dm)
  component$eval()
  expect_equal(names(component$get("ADSL")), c("USUBJID", "AGE", "SEX"))
})

test_that("multiple source domains with SRC_: per-domain mutate and rbind", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = params_multi_source_with_src
  )

  # EXPECT ---------------------------------------------------------------------
  component$assign(x = "LB", value = mock_lb)
  component$assign(x = "XL", value = mock_xl)
  component$eval()
  result <- component$get("ADLB")
  expect_equal(nrow(result), 2L)
  expect_true("SRC_" %in% names(result))
  expect_equal(sort(result$SRC_), c("LB", "XL"))
})
