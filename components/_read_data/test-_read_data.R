# Parameter contracts for the _read_data component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_single_sdtm_domain <- list(
  connector_path_expr = '"_connector.yml"',
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
  connector_path_expr = '"_connector.yml"',
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
  connector_path_expr = '"_connector.yml"',
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
  connector_path_expr = '"_connector.yml"',
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
  connector_path_expr = 'here::here("_connector.yml")',
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
  connector_path_expr = '"_connector.yml"',
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "MDCOL",
      data_type = "metadata",
      keep_vars = "PARAMCD"
    )
  )
)

# mocks ------------------------------------------------------------------------
mock_dm <- data.frame(
  ARM = "A",
  STUDYID = "S001",
  USUBJID = "U001",
  stringsAsFactors = FALSE
)
mock_dm_vaccine <- data.frame(
  ARM = "A",
  STUDYID = "S001",
  USUBJID = "U001",
  stringsAsFactors = FALSE
)
mock_adsl <- data.frame(
  ARM = "A",
  STUDYID = "S001",
  USUBJID = "U001",
  stringsAsFactors = FALSE
)
mock_mdcol <- data.frame(PARAMCD = "ALT", stringsAsFactors = FALSE)

# inject_connector_mock patches the component's test function inside the callr
# session so that connector::connect() returns a mock object instead of
# attempting a real connection. This is necessary because the connector package
# is not available in CI and because unit tests should not depend on live data
# infrastructure.
#
# In a live setting, connector::connect(config = <path>) reads a connector
# configuration file and returns a connection object used to access SDTM, ADaM,
# and metadata datasets. See the mighty vignette for details:
# https://novonordisk-opensource.github.io/mighty/articles/connect_to_data.html
inject_connector_mock <- function(component, mock_cnt) {
  component$.__enclos_env__$private$.session$run(
    func = function(mock) {
      assign(".mock_cnt", mock, envir = globalenv())
      unlockBinding(".test_fn", globalenv())
      fn_str <- get(".test_fn", envir = globalenv())
      fn_str <- sub(
        "connector::connect(",
        "(function(...) get('.mock_cnt', envir = globalenv()))(",
        fn_str,
        fixed = TRUE
      )
      assign(".test_fn", fn_str, envir = globalenv())
      lockBinding(".test_fn", globalenv())
    },
    args = list(mock = mock_cnt)
  )
}

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
    'cnt <- connector::connect(config = "_connector.yml")',
    fixed = TRUE
  )
  expect_match(
    rendered,
    "DM <-  cnt$sdtm$read_cnt(tolower('DM'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(ARM, STUDYID, USUBJID)", fixed = TRUE)

  # COVERAGE -------------------------------------------------------------------
  inject_connector_mock(
    component,
    list(sdtm = list(read_cnt = function(x) mock_dm))
  )
  component$eval()
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
  inject_connector_mock(
    component,
    list(
      adam = list(read_cnt = function(x) mock_adsl),
      sdtm = list(read_cnt = function(x) mock_dm)
    )
  )
  component$eval()
  result_adsl <- component$get("ADSL")
  result_dm <- component$get("DM")
  expect_equal(names(result_adsl), names(mock_adsl))
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
  inject_connector_mock(
    component,
    list(
      sdtm = list(read_cnt = function(x) {
        if (x == "dm") mock_dm else mock_dm_vaccine
      })
    )
  )
  component$eval()
  result_dm <- component$get("DM")
  result_dm_vaccine <- component$get("DM_VACCINE")
  expect_equal(names(result_dm), c("ARM", "STUDYID", "USUBJID"))
  expect_equal(names(result_dm_vaccine), c("ARM", "STUDYID", "USUBJID"))
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
  inject_connector_mock(
    component,
    list(adam = list(read_cnt = function(x) mock_adsl))
  )
  component$eval()
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
    'cnt <- connector::connect(config = here::here("_connector.yml"))',
    fixed = TRUE
  )

  # COVERAGE -------------------------------------------------------------------
  inject_connector_mock(
    component,
    list(sdtm = list(read_cnt = function(x) mock_dm))
  )
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
  inject_connector_mock(
    component,
    list(metadata = list(read_cnt = function(x) mock_mdcol))
  )
  component$eval()
})
