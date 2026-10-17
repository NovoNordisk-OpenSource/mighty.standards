# Parameter contracts for the _init_domain component are documented in the mighty
# vignette: https://novonordisk-opensource.github.io/mighty/articles/special_components.html

# params -----------------------------------------------------------------------
params_single_source <- list(
  self = "ADSL",
  keep_vars = "USUBJID, AGE, SEX",
  source_domain_rbind = "DM",
  src_mutations = list()
)

params_multi_source_with_src <- list(
  self = "ADLB",
  keep_vars = "LBTESTCD, LBSTRESN, SRC_, STUDYID, USUBJID",
  source_domain_rbind = "rbind(LB,\nLB_METABOLIC)",
  src_mutations = list(
    list(domain = "LB"),
    list(domain = "LB_METABOLIC")
  )
)

# data -------------------------------------------------------------------------
test_subjects <- c("01-701-1015", "01-701-1023", "01-701-1028")

dm <- pharmaversesdtm::dm[
  pharmaversesdtm::dm$USUBJID %in% test_subjects,
  c("USUBJID", "AGE", "SEX")
]

lb <- pharmaversesdtm::lb[
  pharmaversesdtm::lb$LBTESTCD == "ALT" &
    pharmaversesdtm::lb$USUBJID %in% test_subjects,
  c("STUDYID", "USUBJID", "LBTESTCD", "LBSTRESN")
]

lb_metabolic <- pharmaversesdtm::lb_metabolic[
  pharmaversesdtm::lb_metabolic$LBTESTCD == "INSULIN" &
    pharmaversesdtm::lb_metabolic$USUBJID %in% test_subjects,
  c("STUDYID", "USUBJID", "LBTESTCD", "LBSTRESN")
]

# tests ------------------------------------------------------------------------

test_that("single source domain: select and convert_blanks_to_na without src mutations", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = params_single_source
  )
  rendered <- paste(component$code, collapse = "\n")

  # EXPECT ---------------------------------------------------------------------
  expect_no_match(rendered, "dplyr::mutate(SRC_", fixed = TRUE)
  component$assign(x = "DM", value = dm)
  component$eval()
  expect_equal(names(component$get("ADSL")), c("USUBJID", "AGE", "SEX"))
})

test_that("multiple source domains with SRC_: per-domain mutate and rbind", {
  # SETUP ----------------------------------------------------------------------
  component <- mighty.component::get_test_component(
    component = "mighty_init_domain.mustache",
    params = params_multi_source_with_src
  )

  # EXPECT ---------------------------------------------------------------------
  component$assign(x = "LB", value = lb)
  component$assign(x = "LB_METABOLIC", value = lb_metabolic)
  component$eval()
  result <- component$get("ADLB")
  expect_true(nrow(result) > 0L)
  expect_true("SRC_" %in% names(result))
  expect_true("LB" %in% result$SRC_)
  expect_true("LB_METABOLIC" %in% result$SRC_)
})
