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

test_that("final program: rows sorted by all keys, only keep_vars columns in keep_vars order", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "STUDYID,\nUSUBJID",
      keep_vars = "USUBJID,\nSTUDYID,\nARM",
      file_ext = "parquet"
    )
  )

  ADSL <- data.frame(
    ARM = c("ARM B", "ARM A", "ARM C"),
    STUDYID = c("STUDY2", "STUDY1", "STUDY1"),
    USUBJID = c("01", "02", "01"),
    SRC_ = c("DM", "DM", "DM")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- as.data.frame(connector::connect(path)$adam$read_cnt(
    "adsl.parquet"
  ))
  expected <- data.frame(
    USUBJID = c("01", "02", "01"),
    STUDYID = c("STUDY1", "STUDY1", "STUDY2"),
    ARM = c("ARM C", "ARM A", "ARM B")
  )

  expect_equal(result, expected)
})

test_that("no row order: rows are written in their input order", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = NULL,
      keep_vars = "USUBJID,\nARM",
      file_ext = "parquet"
    )
  )

  ADSL <- data.frame(
    USUBJID = c("02", "01"),
    ARM = c("ARM B", "ARM A")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- as.data.frame(connector::connect(path)$adam$read_cnt(
    "adsl.parquet"
  ))
  expected <- data.frame(
    USUBJID = c("02", "01"),
    ARM = c("ARM B", "ARM A")
  )

  expect_equal(result, expected)
})

test_that("intermediate program: all columns kept in input order, rows still sorted", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "USUBJID",
      keep_vars = NULL,
      file_ext = "parquet"
    )
  )

  ADSL <- data.frame(
    ARM = c("ARM B", "ARM A"),
    USUBJID = c("02", "01"),
    SRC_ = c("DM", "DM")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- as.data.frame(connector::connect(path)$adam$read_cnt(
    "adsl.parquet"
  ))
  expected <- data.frame(
    ARM = c("ARM A", "ARM B"),
    USUBJID = c("01", "02"),
    SRC_ = c("DM", "DM")
  )

  expect_equal(result, expected)
})

test_that("unavailable column in the middle: commented-out column is left out, no error", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "USUBJID",
      keep_vars = "\nUSUBJID,\n# ARM,\nAGE\n",
      file_ext = "parquet"
    )
  )

  ADSL <- data.frame(
    AGE = c(30, 25),
    USUBJID = c("02", "01")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- as.data.frame(connector::connect(path)$adam$read_cnt(
    "adsl.parquet"
  ))
  expected <- data.frame(
    USUBJID = c("01", "02"),
    AGE = c(25, 30)
  )

  expect_equal(result, expected)
})

test_that("unavailable column last: commented-out last line is left out, no error", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "USUBJID",
      keep_vars = "\nUSUBJID,\nAGE\n# ARM\n",
      file_ext = "parquet"
    )
  )

  ADSL <- data.frame(
    AGE = c(30, 25),
    USUBJID = c("02", "01")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- as.data.frame(connector::connect(path)$adam$read_cnt(
    "adsl.parquet"
  ))
  expected <- data.frame(
    USUBJID = c("01", "02"),
    AGE = c(25, 30)
  )

  expect_equal(result, expected)
})

test_that("file_ext: dataset is written with the given extension", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "USUBJID",
      keep_vars = "USUBJID,\nARM",
      file_ext = "rds"
    )
  )

  ADSL <- data.frame(
    USUBJID = c("02", "01"),
    ARM = c("ARM B", "ARM A")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  comp$eval()

  result <- connector::connect(path)$adam$read_cnt("adsl.rds")
  expected <- data.frame(
    USUBJID = c("01", "02"),
    ARM = c("ARM A", "ARM B")
  )

  expect_equal(result, expected)
})

test_that("keep_vars column missing from the data and not commented out: raises out of bounds subscript error", {
  path <- local_connector(list(adam = list()))

  comp <- mighty.component::get_test_component(
    component = "mighty_write_data.mustache",
    params = list(
      domain = "ADSL",
      row_order_vars = "USUBJID",
      keep_vars = "USUBJID,\nARM",
      file_ext = "parquet"
    ),
    check_coverage = FALSE
  )

  ADSL <- data.frame(
    USUBJID = c("02", "01")
  )
  comp$assign("ADSL", ADSL)
  comp$assign("cnt", connector::connect(path))

  # mighty comments out unavailable columns before rendering; this test records the template's behavior if it does not
  expect_error(
    comp$eval(),
    class = "vctrs_error_subscript_oob"
  )
})
