# Parameter contracts for the _read_data component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
test_connector_path_expr <- '"test_study/_connector.yml"'

params_single_sdtm_domain <- list(
  connector_path_expr = test_connector_path_expr,
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "DM",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    )
  )
)

params_self_domain <- list(
  connector_path_expr = test_connector_path_expr,
  domains = list(
    list(
      is_current_domain = TRUE,
      domain_name = "ADSL",
      data_type = "adam",
      keep_vars = "ARM, STUDYID, USUBJID"
    ),
    list(
      is_current_domain = FALSE,
      domain_name = "DM",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    )
  )
)

params_multiple_sdtm_domains <- list(
  connector_path_expr = test_connector_path_expr,
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "DM",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    ),
    list(
      is_current_domain = FALSE,
      domain_name = "DM_VACCINE",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    )
  )
)

params_adam_cross_domain <- list(
  connector_path_expr = test_connector_path_expr,
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "ADSL",
      data_type = "adam",
      keep_vars = "ARM, USUBJID"
    )
  )
)

params_expr_path <- list(
  connector_path_expr = 'here::here("test_study/_connector.yml")',
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "DM",
      data_type = "sdtm",
      keep_vars = "USUBJID"
    )
  )
)

params_metadata_domain <- list(
  connector_path_expr = test_connector_path_expr,
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "MDCOL",
      data_type = "metadata",
      keep_vars = "PARAMCD"
    )
  )
)

# tests ------------------------------------------------------------------------

test_that("single SDTM domain: renders connector setup and read with column selection", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_single_sdtm_domain
  )

  # EXPECT ---------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("DM")), c("ARM", "STUDYID", "USUBJID"))
})

test_that("self domain: renders read without column selection for self, with selection for other", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_self_domain
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  # self domain must NOT have a select call immediately after it
  expect_no_match(
    rendered,
    "read_cnt(tolower('ADSL')) |>\n  dplyr::select",
    fixed = TRUE
  )
  component$eval()
  result_adsl <- component$get("ADSL")
  result_dm <- component$get("DM")
  # Given that is_current_domain = TRUE, keep vars of params_self_domain
  # should be ignored. Instead it should keep all vars from the adsl
  # dataset, which includes BMIBL.
  expect_true("BMIBL" %in% names(result_adsl))
  expect_equal(names(result_dm), c("ARM", "STUDYID", "USUBJID"))
})

test_that("multiple SDTM domains: renders one read block per domain", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_multiple_sdtm_domains
  )

  # EXPECT ---------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("DM")), c("ARM", "STUDYID", "USUBJID"))
  expect_equal(names(component$get("DM_VACCINE")), c("ARM", "STUDYID", "USUBJID"))
})

test_that("ADaM cross-domain: renders read via adam connector with column selection", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_adam_cross_domain
  )

  # EXPECT ---------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("ADSL")), c("ARM", "USUBJID"))
})

test_that("bare R expression path: renders connector config without quoting", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_expr_path
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    'cnt <- connector::connect(config = here::here("test_study/_connector.yml"))',
    fixed = TRUE
  )
  # here::here() resolves to the repo root in the callr session, not the
  # component folder. Patch the rendered code to use a literal path so $eval()
  # can exercise all template lines without depending on here's project-root logic.
  component$.__enclos_env__$private$.session$run(function() {
    unlockBinding(".test_fn", globalenv())
    fn_str <- get(".test_fn", envir = globalenv())
    fn_str <- sub(
      'here::here("test_study/_connector.yml")',
      '"test_study/_connector.yml"',
      fn_str,
      fixed = TRUE
    )
    assign(".test_fn", fn_str, envir = globalenv())
    lockBinding(".test_fn", globalenv())
  })
  component$eval()
})

test_that("metadata domain: renders read via metadata connector with column selection", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = params_metadata_domain
  )

  # EXPECT ---------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("MDCOL")), "PARAMCD")
})
