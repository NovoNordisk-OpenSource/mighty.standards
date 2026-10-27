run_ady <- function(
  data,
  domain = "adae",
  variable = "ADY",
  date = "ADT",
  env = parent.frame()
) {
  comp <- mighty.component::get_test_component(
    component = "ady.mustache",
    params = list(domain = domain, variable = variable, date = date),
    teardown_env = env
  )
  comp$assign(domain, data)
  comp$eval()
  comp$get(domain)
}

# Checks the expected values and clause 8: numeric, and whole numbers other than 0 or missing
expect_ady <- function(data, expected, date = "ADT", env = parent.frame()) {
  result <- run_ady(data, date = date, env = env)$ADY

  expect_equal(result, expected)
  expect_true(is.numeric(result))
  expect_false(any(is.nan(result)))
  expect_true(all(result == trunc(result), na.rm = TRUE))
  expect_false(any(result == 0, na.rm = TRUE))
}

utc <- function(x) as.POSIXct(x, tz = "UTC")

test_that("counts both days across a leap day", {
  expect_ady(
    data.frame(TRTSDT = as.Date("2024-02-28"), ADT = as.Date("2024-03-01")),
    expected = 3
  )
})

test_that("TRTSDT is day 1 and the day before is -1, with no day 0", {
  expect_ady(
    data.frame(
      TRTSDT = as.Date("2024-03-01"),
      ADT = seq(as.Date("2024-02-27"), as.Date("2024-03-04"), by = "day")
    ),
    expected = c(-3, -2, -1, 1, 2, 3, 4)
  )
})

test_that("datetime after TRTSDT: only the calendar date counts", {
  expect_ady(
    data.frame(
      TRTSDT = as.Date("2024-03-01"),
      ADTM = utc(c("2024-03-01 23:59", "2024-03-02 00:00"))
    ),
    date = "ADTM",
    expected = c(1, 2)
  )
})

test_that("datetime before TRTSDT: only the calendar date counts", {
  expect_ady(
    data.frame(
      TRTSDT = as.Date("2024-03-01"),
      ADTM = utc(c("2024-02-29 23:59", "2024-02-29 00:00", "2024-02-28 23:59"))
    ),
    date = "ADTM",
    expected = c(-1, -1, -2)
  )
})

test_that("missing date gives a missing result", {
  expect_ady(
    data.frame(TRTSDT = as.Date("2024-03-01"), ADT = as.Date(NA)),
    expected = NA_real_
  )
})

test_that("missing TRTSDT gives a missing result", {
  expect_ady(
    data.frame(TRTSDT = as.Date(NA), ADT = as.Date("2024-03-05")),
    expected = NA_real_
  )
})

test_that("both missing gives a missing result", {
  expect_ady(
    data.frame(TRTSDT = as.Date(NA), ADT = as.Date(NA)),
    expected = NA_real_
  )
})

test_that("missing datetime gives a missing result", {
  expect_ady(
    data.frame(
      TRTSDT = as.Date("2024-03-01"),
      ADTM = .POSIXct(NA_real_, tz = "UTC")
    ),
    date = "ADTM",
    expected = NA_real_
  )
})

test_that("uses the domain, variable and date parameters", {
  result <- run_ady(
    data.frame(TRTSDT = as.Date("2024-03-01"), AENDT = as.Date("2024-03-03")),
    domain = "adlb",
    variable = "AENDY",
    date = "AENDT"
  )

  expect_equal(result$AENDY, 3)
})

test_that("reads only TRTSDT and the date column", {
  data <- data.frame(
    TRTSDT = as.Date("2024-03-01"),
    ADT = as.Date(c("2024-02-29", "2024-03-05"))
  )
  decoys <- data.frame(
    ADTM = utc(c("2020-01-01 00:00", "2030-01-01 00:00")),
    TRTEDT = as.Date(c("2020-01-01", NA)),
    RFSTDTC = c("2020-01-01", "2030-01-01"),
    ASTDT = as.Date(c("2020-01-01", "2030-01-01"))
  )

  expect_equal(run_ady(cbind(data, decoys))$ADY, run_ady(data)$ADY)

  depends <- mighty.component::get_rendered_component(
    component = "ady.mustache",
    params = list(domain = "adae", variable = "ADY", date = "ADT")
  )$depends
  expect_setequal(
    paste(depends$domain, depends$column),
    c("adae TRTSDT", "adae ADT")
  )
})

test_that("changes only the new variable", {
  data <- data.frame(
    USUBJID = c("02", "01", "01"),
    AVISIT = factor(c("W2", "W1", "W1"), levels = c("W1", "W2")),
    AVAL = c(1.5, NA, 3),
    TRTSDT = as.Date("2024-03-01"),
    ADT = as.Date(c("2024-03-05", NA, "2024-02-20")),
    ADTM = utc(c("2024-03-05 10:00", NA, "2024-02-20 08:30"))
  )
  attr(data$AVAL, "label") <- "Analysis value"

  result <- run_ady(data)

  expect_identical(names(result), c(names(data), "ADY"))
  expect_identical(result[names(data)], data)
})

test_that("@type column: keeps the rows and their order", {
  data <- data.frame(
    ROW = 1:6,
    TRTSDT = as.Date("2024-03-01"),
    ADT = as.Date(c(
      "2024-03-04",
      "2024-02-28",
      NA,
      "2024-03-01",
      "2024-02-28",
      "2024-03-02"
    ))
  )

  result <- run_ady(data)

  expect_identical(result$ROW, data$ROW)
  expect_equal(result$ADY, c(4, -2, NA, 1, -2, 2))
})

test_that("gives no warnings and no messages for valid input", {
  # The test component runs in a separate R session, which hides its warnings and messages.
  # The rendered code runs here directly, because the mighty.component functions print their own messages.
  render <- function(date) {
    mighty.component::get_rendered_component(
      component = "ady.mustache",
      params = list(domain = "adae", variable = "ADY", date = date)
    )$code
  }
  eval_locally <- function(code, data) {
    env <- new.env()
    env$adae <- data
    eval(parse(text = code), envir = env)
  }

  code_date <- render("ADT")
  code_datetime <- render("ADTM")

  expect_silent(eval_locally(
    code_date,
    data.frame(
      TRTSDT = as.Date("2024-03-01"),
      ADT = as.Date(c("2024-02-29", "2024-03-05", NA))
    )
  ))
  expect_silent(eval_locally(
    code_datetime,
    data.frame(
      TRTSDT = as.Date(c("2024-03-01", NA)),
      ADTM = utc(c("2024-02-29 23:59", "2024-03-05 10:00"))
    )
  ))
})
