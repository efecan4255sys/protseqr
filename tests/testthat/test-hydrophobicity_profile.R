test_that("calc_hydro_profile works correctly", {
  # Test with known short sequence: "AAAAA" with window_size = 3
  # A = 1.8, mean of 3 A's is 1.8
  res <- calc_hydro_profile("AAAAA", window_size = 3)
  expect_equal(res, c(1.8, 1.8, 1.8))

  # Error on even window size
  expect_error(calc_hydro_profile("AAAAA", window_size = 4))

  # Error when sequence is shorter than window
  expect_error(calc_hydro_profile("AA", window_size = 5))
})
