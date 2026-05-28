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
    component = "_read_data.mustache",
    params = params_single_sdtm_domain
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    'cnt <- connector::connect(config = "test_study/_connector.yml")',
    fixed = TRUE
  )
  expect_match(
    rendered,
    "DM <-  cnt$sdtm$read_cnt(tolower('DM'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(ARM, STUDYID, USUBJID)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("DM")), c("ARM", "STUDYID", "USUBJID"))
})

test_that("self domain: renders read without column selection for self, with selection for other", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_self_domain
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "ADSL <-  cnt$adam$read_cnt(tolower('ADSL'))",
    fixed = TRUE
  )
  expect_match(
    rendered,
    "DM <-  cnt$sdtm$read_cnt(tolower('DM'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(ARM, STUDYID, USUBJID)", fixed = TRUE)
  # self domain must NOT have a select call immediately after it
  expect_no_match(
    rendered,
    "read_cnt(tolower('ADSL')) |>\n  dplyr::select",
    fixed = TRUE
  )

  # COVERAGE -------------------------------------------------------------------
  component$eval()
  result_adsl <- component$get("ADSL")
  result_dm <- component$get("DM")
  # Given that is_current_domain = TRUE, keep vars of params_self_domain
  # should be ignored. Instead it should keep all vars from the mock_adsl
  # dataset, which includes BMIBL.
  expect_true("BMIBL" %in% names(result_adsl))
  expect_equal(names(result_dm), c("ARM", "STUDYID", "USUBJID"))
})

test_that("multiple SDTM domains: renders one read block per domain", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_multiple_sdtm_domains
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "DM <-  cnt$sdtm$read_cnt(tolower('DM'))",
    fixed = TRUE
  )
  expect_match(
    rendered,
    "DM_VACCINE <-  cnt$sdtm$read_cnt(tolower('DM_VACCINE'))",
    fixed = TRUE
  )
  # both domains select the same columns so they can be row-bound in _init_domain
  expect_equal(
    sum(
      gregexpr("dplyr::select(ARM, STUDYID, USUBJID)", rendered, fixed = TRUE)[[
        1
      ]] >
        0
    ),
    2L
  )

  # COVERAGE -------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("DM")), c("ARM", "STUDYID", "USUBJID"))
  expect_equal(names(component$get("DM_VACCINE")), c("ARM", "STUDYID", "USUBJID"))
})

test_that("ADaM cross-domain: renders read via adam connector with column selection", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_adam_cross_domain
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "ADSL <-  cnt$adam$read_cnt(tolower('ADSL'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(ARM, USUBJID)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("ADSL")), c("ARM", "USUBJID"))
})

test_that("bare R expression path: renders connector config without quoting", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_expr_path
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    'cnt <- connector::connect(config = here::here("test_study/_connector.yml"))',
    fixed = TRUE
  )

  # COVERAGE -------------------------------------------------------------------
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
    component = "_read_data.mustache",
    params = params_metadata_domain
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_match(
    rendered,
    "MDCOL <-  cnt$metadata$read_cnt(tolower('MDCOL'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(PARAMCD)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  component$eval()
  expect_equal(names(component$get("MDCOL")), "PARAMCD")
})
