# Units tests for subset_data( )
library(readxl)
library(tidyverse)

# initialize test data and vectors
data <- read_excel(
  here::here("analysis/data/41586_2010_BFnature08920_MOESM90_ESM.xls"),
  skip = 6)
names(data) <- str_replace_all(names(data), c(" " = ".",
                                              "[(]" = ".",
                                              "[)]" = ""))

################################################################################

# smoke tests
# initialize test vectors
drop_genera_1 <- (c("Elephantulus", "Orcinus")) # positive control (in data)
drop_genera_2 <- (c("Zaglossus", "Caluromys")) # positive control (in data)
drop_genera_3 <- "Elephantulus" # positive control (in data)
drop_genera_4 <- (c("Elephantulus", "")) # positive control (in data)
drop_genera_5 <- (c("Pandas", "Koalas")) # negative control (not in data)

test_that("subset data returns df of expected length for valid vectors", {
  expect_equal(nrow(subset_data(data, drop_genera_1)), 631)
  expect_equal(nrow(subset_data(data, drop_genera_2)), 636)
  expect_equal(nrow(subset_data(data, drop_genera_3)), 632)
  expect_equal(nrow(subset_data(data, drop_genera_4)), 632)
  expect_equal(nrow(subset_data(data, drop_genera_5)), nrow(data))
})
#> smoke tests passed

# boundary tests
# initialize test variables
drop_genera_6 <- (c(1, 2))
drop_genera_7 <- (c(2, "Elephantulus"))
drop_genera_8 <- NA
data_1 <- "not a dataframe or tibble"
data_2 <- tibble(0)

test_that("subset data works for non-alphabet vectors", {
  expect_equal(nrow(subset_data(data, drop_genera_6)), 638)
  expect_equal(nrow(subset_data(data, drop_genera_7)), 632)
  expect_equal(nrow(subset_data(data, drop_genera_8)), 638)
})
#> boundary tests (numeric vectors) passed

test_that("sebset data throws errors for wrong data formats", {
  expect_error(subset_data(data_1, drop_genera_1),
               "applied to an object of class")
  expect_error(subset_data(data_2, drop_genera_1), "not found")
})
#> boundary tests (data formats) passed

test_that("subset data thows error for missing required parameters", {
  expect_error(subset_data(data), "missing")
  expect_error(subset_data(drop_genera_1), "applied to an object of class")
})
#> boundary tests (missing parameters) passed
