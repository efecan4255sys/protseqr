test_that("calc_hydrophobicity dogru skor dondurur", {
  expect_type(calc_hydrophobicity("MKVLWA"), "double")
  expect_equal(calc_hydrophobicity("A"), 1.8)
})

test_that("Gecersiz amino asitlerde hata verir", {
  expect_error(calc_hydrophobicity("MKVXWA"))
  expect_error(calc_hydrophobicity(123))
})
