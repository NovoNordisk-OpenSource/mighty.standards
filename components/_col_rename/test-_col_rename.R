# Parameter contracts for the _col_rename component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_basic <- list(
  self = "ADLB",
  rename_var = "SRCSEQ",
  source_var = "LBSEQ"
)

params_domain_level <- list(
  self = "ADSL",
  rename_var = "TRTP",
  source_var = "ARM"
)

# tests ------------------------------------------------------------------------

test_that("basic rename: renders rename replacing source column with new name", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_rename.mustache",
    params = params_basic
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADLB <- ADLB |> dplyr::rename(SRCSEQ = LBSEQ)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = data.frame(LBSEQ = 1L, stringsAsFactors = FALSE))
  component$eval()
})

test_that("domain-level rename: renders rename on different domain and columns", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_rename.mustache",
    params = params_domain_level
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADSL <- ADSL |> dplyr::rename(TRTP = ARM)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADSL", value = data.frame(ARM = "A", stringsAsFactors = FALSE))
  component$eval()
})
