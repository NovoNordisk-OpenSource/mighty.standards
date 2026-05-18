test_that("dummy", {
  dummy <- mighty.component::get_test_component(
    component = "dummy.mustache",
    params = list(domain = "df", variable = "y", value = 1)
  )

  dummy$assign(x = "df", value = data.frame(x = "a"))

  df <- dummy$eval()$get("df")

  expect_equal(df, data.frame(x = "a", y = 1))
})
