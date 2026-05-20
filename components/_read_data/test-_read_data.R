params_simple <- list(
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

params_simple_self_domain <- list(
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

mock_dm <- data.frame(ARM = "A", STUDYID = "S001", USUBJID = "U001", stringsAsFactors = FALSE)
mock_adsl <- data.frame(ARM = "A", STUDYID = "S001", USUBJID = "U001", stringsAsFactors = FALSE)

inject_connector_mock <- function(component, mock_cnt) {
  component$.__enclos_env__$private$.session$run(
    func = function(mock) {
      if (requireNamespace("connector", quietly = TRUE)) {
        assignInNamespace("connect", function(...) mock, ns = "connector")
      } else {
        fake_ns <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
        fake_ns$connect <- function(...) mock
        assign(
          "connector", fake_ns,
          envir = get(".NamespaceRegistry", envir = asNamespace("base"), inherits = FALSE)
        )
      }
    },
    args = list(mock = mock_cnt)
  )
}

test_that("rendered code contains connector setup and DM read with column selection", {
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_simple
  )

  rendered <- paste(component$code, collapse = "\n")

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

  inject_connector_mock(
    component,
    list(sdtm = list(read_cnt = function(x) mock_dm))
  )
  component$eval()
})

test_that("rendered code contains connector setup and ADSL read with column selection", {
  component <- mighty.component::get_test_component(
    component = "_read_data.mustache",
    params = params_simple_self_domain
  )

  rendered <- paste(component$code, collapse = "\n")

  expect_match(
    rendered,
    'cnt <- connector::connect(config = "_connector.yml")',
    fixed = TRUE
  )
  expect_match(
    rendered,
    "ADSL <-  cnt$adam$read_cnt(tolower('ADSL'))",
    fixed = TRUE
  )
  expect_match(rendered, "dplyr::select(ARM, STUDYID, USUBJID)", fixed = TRUE)

  inject_connector_mock(
    component,
    list(
      adam = list(read_cnt = function(x) mock_adsl),
      sdtm = list(read_cnt = function(x) mock_dm)
    )
  )
  component$eval()
})
