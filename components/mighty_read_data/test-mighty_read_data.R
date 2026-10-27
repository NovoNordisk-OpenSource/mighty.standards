# Write datasets as .rds files, one folder per data type, and return the path to a _connector.yml for them
local_connector <- function(datasets, .local_envir = parent.frame()) {
  temp_dir <- withr::local_tempdir(.local_envir = .local_envir)
  datasources <- lapply(names(datasets), function(dtype) {
    type_dir <- file.path(temp_dir, dtype)
    dir.create(type_dir)
    for (name in names(datasets[[dtype]])) {
      saveRDS(
        datasets[[dtype]][[name]],
        file.path(type_dir, paste0(tolower(name), ".rds"))
      )
    }
    list(name = dtype, backend = list(type = "connector_fs", path = type_dir))
  })
  cfg_path <- file.path(temp_dir, "_connector.yml")
  yaml::write_yaml(list(datasources = datasources), cfg_path)
  cfg_path
}

test_that("other domain: only keep_vars columns are kept, in keep_vars order, rows and values unchanged", {
  dm <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B"),
    SEX = c("M", "F")
  )
  path <- local_connector(list(sdtm = list(DM = dm)))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = deparse(path),
      domains = list(
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("ARM", "USUBJID")
        )
      )
    )
  )

  comp$eval()

  result <- comp$get("DM")
  expected <- data.frame(
    ARM = c("ARM A", "ARM B"),
    USUBJID = c("01", "02")
  )

  expect_equal(result, expected)
})

test_that("current and other domain: current domain is read whole and other domain is selected", {
  adsl <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B"),
    SEX = c("M", "F")
  )
  dm <- data.frame(
    USUBJID = c("01", "02"),
    STUDYID = c("STUDY1", "STUDY1"),
    AGE = c(25, 30)
  )
  path <- local_connector(list(
    adam = list(ADSL = adsl),
    sdtm = list(DM = dm)
  ))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = deparse(path),
      domains = list(
        list(
          is_current_domain = TRUE,
          domain_name = "ADSL",
          data_type = "adam",
          keep_vars = c("USUBJID", "ARM")
        ),
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("USUBJID", "STUDYID")
        )
      )
    )
  )

  comp$eval()

  result_adsl <- comp$get("ADSL")
  
  # Includes SEX although it is not in keep_vars, because the current domain is read with all columns
  expected_adsl <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B"),
    SEX = c("M", "F")
  )
  expect_equal(result_adsl, expected_adsl)

  result_dm <- comp$get("DM")
  expected_dm <- data.frame(
    USUBJID = c("01", "02"),
    STUDYID = c("STUDY1", "STUDY1")
  )
  expect_equal(result_dm, expected_dm)
})

test_that("data_type routing: reads each domain from its own datasource and selects columns", {
  dm <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B"),
    SEX = c("M", "F")
  )
  adsl <- data.frame(
    USUBJID = c("01", "02"),
    TRT01P = c("ARM A", "ARM B"),
    AGE = c(25, 30)
  )
  mdcol <- data.frame(
    DOMAIN = c("DM", "ADSL"),
    VAR = c("USUBJID", "USUBJID"),
    LABEL = c("Unique Subject Identifier", "Unique Subject Identifier")
  )
  path <- local_connector(list(
    sdtm = list(DM = dm),
    adam = list(ADSL = adsl),
    metadata = list(MDCOL = mdcol)
  ))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = deparse(path),
      domains = list(
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("USUBJID", "ARM")
        ),
        list(
          is_current_domain = FALSE,
          domain_name = "ADSL",
          data_type = "adam",
          keep_vars = c("USUBJID", "TRT01P")
        ),
        list(
          is_current_domain = FALSE,
          domain_name = "MDCOL",
          data_type = "metadata",
          keep_vars = c("DOMAIN", "VAR")
        )
      )
    )
  )

  comp$eval()

  result_dm <- comp$get("DM")
  expected_dm <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B")
  )
  expect_equal(result_dm, expected_dm)

  result_adsl <- comp$get("ADSL")
  expected_adsl <- data.frame(
    USUBJID = c("01", "02"),
    TRT01P = c("ARM A", "ARM B")
  )
  expect_equal(result_adsl, expected_adsl)

  result_mdcol <- comp$get("MDCOL")
  expected_mdcol <- data.frame(
    DOMAIN = c("DM", "ADSL"),
    VAR = c("USUBJID", "USUBJID")
  )
  expect_equal(result_mdcol, expected_mdcol)
})

test_that("bare R expression path: evaluates path expression and reads domain correctly", {
  dm <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B"),
    SEX = c("M", "F")
  )
  path <- local_connector(list(sdtm = list(DM = dm)))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = sprintf(
        "file.path(%s, '_connector.yml')",
        deparse(dirname(path))
      ),
      domains = list(
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("USUBJID", "ARM")
        )
      )
    )
  )

  comp$eval()

  result <- comp$get("DM")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B")
  )

  expect_equal(result, expected)
})

test_that("connector object: `cnt` is left in the environment for mighty_write_data", {
  dm <- data.frame(USUBJID = c("01", "02"))
  path <- local_connector(list(sdtm = list(DM = dm)))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = deparse(path),
      domains = list(
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("USUBJID")
        )
      )
    )
  )

  comp$eval()

  # mighty_write_data (last step of every program) writes via `cnt$adam$write_cnt()`
  # connector 1.0.0.9004 (r-universe dev) is S7 with class "connector::connectors"; CRAN 1.0.0 is S3 "connectors"
  expect_s3_class(comp$get("cnt"), c("connectors", "connector::connectors"))
})

test_that("missing keep_vars column: raises out of bounds subscript error", {
  dm <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B")
  )
  path <- local_connector(list(sdtm = list(DM = dm)))

  comp <- mighty.component::get_test_component(
    component = "mighty_read_data.mustache",
    params = list(
      connector_path_expr = deparse(path),
      domains = list(
        list(
          is_current_domain = FALSE,
          domain_name = "DM",
          data_type = "sdtm",
          keep_vars = c("USUBJID", "NONEXISTENT")
        )
      )
    ),
    check_coverage = FALSE
  )

  # mighty removes unavailable columns before rendering; this test records the template's behavior if it does not
  expect_error(
    comp$eval(),
    class = "vctrs_error_subscript_oob"
  )
})
