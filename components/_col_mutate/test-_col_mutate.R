# Parameter contracts for the _col_mutate component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_basic <- list(
  self = "ADLB",
  rename_var = "AVAL",
  source_var = "LBSTRESN"
)

params_same_name <- list(
  self = "ADSL",
  rename_var = "AGE",
  source_var = "AGE"
)

# tests ------------------------------------------------------------------------

test_that("basic copy: renders mutate assigning source column to new column", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_mutate.mustache",
    params = params_basic
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADLB <- ADLB |> dplyr::mutate(AVAL = LBSTRESN)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADLB", value = data.frame(LBSTRESN = 1.5, stringsAsFactors = FALSE))
  component$eval()
})

test_that("same-name copy: renders mutate with identical source and target column", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_col_mutate.mustache",
    params = params_same_name
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(rendered, "ADSL <- ADSL |> dplyr::mutate(AGE = AGE)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$assign(x = "ADSL", value = data.frame(AGE = 30L, stringsAsFactors = FALSE))
  component$eval()
})
