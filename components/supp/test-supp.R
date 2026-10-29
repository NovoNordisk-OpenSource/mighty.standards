run_supp <- function(
  adae,
  supp,
  qnam = "AESOSP",
  idvar = "AESEQ",
  source = "suppae",
  check_coverage = TRUE
) {
  x <- mighty.component::get_test_component(
    component = "supp.mustache",
    params = list(domain = "adae", source = source, qnam = qnam, idvar = idvar),
    check_coverage = check_coverage
  )
  x$assign(x = "adae", value = adae)
  x$assign(x = source, value = supp)
  expect_silent(x$eval())
  x$get("adae")
}

supp_df <- function(USUBJID, IDVARVAL, QNAM, QVAL, IDVAR = "AESEQ") {
  data.frame(USUBJID, IDVAR, IDVARVAL, QNAM, QVAL)
}

expect_character_cols <- function(df, qnam) {
  for (col in qnam) {
    expect_type(df[[col]], "character")
  }
}

test_that("1: numeric idvar, two QNAMs", {
  # clauses 1, 2, 5, 15
  qnam <- c("AESOSP", "AETRTEM")
  supp <- supp_df(
    c("S1", "S1", "S2"),
    c("1", "2", "1"),
    c("AESOSP", "AETRTEM", "AESOSP"),
    c("Headache", "Y", "Rash")
  )
  adae <- data.frame(USUBJID = c("S1", "S1", "S2"), AESEQ = c(1L, 2L, 1L))

  out <- run_supp(adae, supp, qnam = qnam)

  expected <- data.frame(
    adae,
    AESOSP = c("Headache", NA, "Rash"),
    AETRTEM = c(NA, "Y", NA)
  )
  expect_equal(out, expected)
  expect_character_cols(out, qnam)
})

test_that("2: IDVARVAL is read as a number", {
  # clauses 2, 5
  supp <- supp_df(c("S1", "S1"), c("01", "2.0"), "AESOSP", c("A", "B"))
  adae <- data.frame(USUBJID = "S1", AESEQ = c(1, 2, 10))

  out <- run_supp(adae, supp)

  expect_equal(out, data.frame(adae, AESOSP = c("A", "B", NA)))
  expect_character_cols(out, "AESOSP")
})

test_that("3: character idvar", {
  # clauses 2, 5
  supp <- supp_df("S1", "A1", "AESOSP", "Mild", IDVAR = "AESPID")
  adae <- data.frame(USUBJID = "S1", AESPID = c("A1", "A01", "a1"))

  # the numeric branch is not run for a character idvar; tests 1 and 2 cover it
  out <- run_supp(adae, supp, idvar = "AESPID", check_coverage = FALSE)

  expect_equal(out, data.frame(adae, AESOSP = c("Mild", NA, NA)))
  expect_character_cols(out, "AESOSP")
})

test_that("4: subject-level records (idvar NULL)", {
  # clauses 3, 4, 5; IDVAR and IDVARVAL blank as "" and as missing (clause 10)
  supp <- supp_df(
    c("S1", "S2"),
    c("", NA),
    "RACEOTH",
    c("Other A", "Other B"),
    IDVAR = c("", NA)
  )
  adae <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S2", "S3"),
    AESEQ = c(1, 2, 3, 1, 1)
  )

  out <- run_supp(adae, supp, qnam = "RACEOTH", idvar = NULL, source = "suppdm")

  expect_equal(
    out,
    data.frame(
      adae,
      RACEOTH = c("Other A", "Other A", "Other A", "Other B", NA)
    )
  )
  expect_character_cols(out, "RACEOTH")
})

test_that("5: one SUPP record matches several domain records", {
  # clauses 4, 5
  supp <- supp_df("S1", "G1", "AESOSP", "Headache", IDVAR = "AEGRPID")
  adae <- data.frame(
    USUBJID = "S1",
    AESEQ = c(1, 2, 3),
    AEGRPID = c("G1", "G1", "G2")
  )

  # the numeric branch is not run for a character idvar; tests 1 and 2 cover it
  out <- run_supp(adae, supp, idvar = "AEGRPID", check_coverage = FALSE)

  expect_equal(out, data.frame(adae, AESOSP = c("Headache", "Headache", NA)))
  expect_character_cols(out, "AESOSP")
})

test_that("6: missing idvar value in the domain", {
  # clause 5
  supp <- supp_df("S1", "1", "AESOSP", "Headache")
  adae <- data.frame(USUBJID = c("S1", "S1"), AESEQ = c(NA, 1))

  out <- run_supp(adae, supp)

  expect_equal(out, data.frame(adae, AESOSP = c(NA, "Headache")))
  expect_character_cols(out, "AESOSP")
})

test_that("7: QNAM with no records", {
  # clauses 5, 15
  qnam <- c("AESOSP", "AEXTRA")
  supp <- supp_df("S1", "1", "AESOSP", "Headache")
  adae <- data.frame(USUBJID = "S1", AESEQ = 1)

  out <- run_supp(adae, supp, qnam = qnam)

  expect_equal(
    out,
    data.frame(adae, AESOSP = "Headache", AEXTRA = NA_character_)
  )
  expect_character_cols(out, qnam)
})

test_that("8: source with zero rows", {
  # clauses 5, 15
  qnam <- c("AESOSP", "AETRTEM")
  supp <- supp_df(
    character(0),
    character(0),
    character(0),
    character(0),
    IDVAR = character(0)
  )
  adae <- data.frame(USUBJID = "S1", AESEQ = 1)

  out <- run_supp(adae, supp, qnam = qnam)

  expect_equal(
    out,
    data.frame(adae, AESOSP = NA_character_, AETRTEM = NA_character_)
  )
  expect_character_cols(out, qnam)
})

test_that("9: QNAM text match; other QNAMs are ignored", {
  # clauses 1, 6
  supp <- supp_df(
    "S1",
    "1",
    c("aesosp", "AEOTHER", "AEOTHER"),
    c("Headache", "X", "Z"),
    IDVAR = c("AESEQ", "AEGRPID", "AESEQ")
  )
  adae <- data.frame(USUBJID = "S1", AESEQ = 1)

  out <- run_supp(adae, supp)

  expect_equal(out, data.frame(adae, AESOSP = NA_character_))
  expect_character_cols(out, "AESOSP")
})

test_that("10: QVAL is copied without change", {
  # clauses 7, 15
  supp <- supp_df("S1", c("1", "2", "3"), "AESOSP", c("007", "Mild ", "y"))
  adae <- data.frame(USUBJID = "S1", AESEQ = c(1, 2, 3))

  out <- run_supp(adae, supp)

  expect_equal(out, data.frame(adae, AESOSP = c("007", "Mild ", "y")))
  expect_character_cols(out, "AESOSP")
})

test_that("11: domain with zero rows", {
  # clause 15
  qnam <- c("AESOSP", "AETRTEM")
  supp <- supp_df("S1", "1", "AESOSP", "Headache")
  adae <- data.frame(USUBJID = character(0), AESEQ = numeric(0))

  out <- run_supp(adae, supp, qnam = qnam)

  expect_equal(
    out,
    data.frame(adae, AESOSP = character(0), AETRTEM = character(0))
  )
  expect_character_cols(out, qnam)
})

test_that("depends and outputs match the issue", {
  get_meta <- function(idvar) {
    mighty.component::get_test_component(
      component = "supp.mustache",
      params = list(
        domain = "adae",
        source = "suppae",
        qnam = c("AESOSP", "AETRTEM"),
        idvar = idvar
      ),
      check_coverage = FALSE
    )
  }

  x <- get_meta("AESEQ")
  expect_equal(
    x$depends,
    data.frame(
      domain = c("adae", "adae", "suppae", "suppae", "suppae", "suppae"),
      column = c("USUBJID", "AESEQ", "USUBJID", "IDVARVAL", "QNAM", "QVAL")
    )
  )
  expect_equal(x$outputs, c("AESOSP", "AETRTEM"))

  x <- get_meta(NULL)
  expect_equal(
    x$depends,
    data.frame(
      domain = c("adae", "suppae", "suppae", "suppae"),
      column = c("USUBJID", "USUBJID", "QNAM", "QVAL")
    )
  )
  expect_equal(x$outputs, c("AESOSP", "AETRTEM"))
})

test_that("reads only the depends columns", {
  # no IDVAR, RDOMAIN, STUDYID or QLABEL in the input
  supp <- data.frame(
    USUBJID = "S1",
    IDVARVAL = "1",
    QNAM = "AESOSP",
    QVAL = "Headache"
  )
  adae <- data.frame(USUBJID = "S1", AESEQ = 1)
  expect_equal(run_supp(adae, supp), data.frame(adae, AESOSP = "Headache"))

  supp <- data.frame(USUBJID = "S1", QNAM = "RACEOTH", QVAL = "Other A")
  adae <- data.frame(USUBJID = "S1")
  expect_equal(
    run_supp(adae, supp, qnam = "RACEOTH", idvar = NULL),
    data.frame(adae, RACEOTH = "Other A")
  )
})

test_that("changes only the qnam columns; rows and their order do not change", {
  supp <- supp_df(
    c("S2", "S1", "S1"),
    c("1", "2", "1"),
    "AESOSP",
    c("A", "C", "B")
  )
  adae <- data.frame(
    STUDYID = "ST1",
    USUBJID = c("S2", "S1", "S1"),
    AESEQ = c(1, 2, 1),
    AETERM = c("T1", "T2", "T3"),
    AESOSP_X = "keep"
  )

  out <- run_supp(adae, supp)

  expect_equal(out, data.frame(adae, AESOSP = c("A", "C", "B")))
})
