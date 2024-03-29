test_that("utils_url_remonterletemps works", {
  url <- utils_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
  expect_true(inherits(url, "character"),
              "character url loaded")
})

test_that("utils_normalize_string works", {
  original_string <- "Thïs is à sâmplè strîng with spèciál chàracters!"
  normalized_string <- utils_normalize_string(original_string)
  expect_true(normalized_string == "thisisasamplestringwithspecialcharacters")
})
