test_that("dummy", {
  dummy <- mighty.component::get_test_component(
    component = "dummy.mustache",
    params = list(domain = "df", variable = "y", value = 1)
  )

  df <- data.frame(x = "a", y = 1)

  dummy$assign(x = "df", value = dplyr::select(df, -y))
  dummy$eval()$get("df") |>
    expect_equal(df)
})
