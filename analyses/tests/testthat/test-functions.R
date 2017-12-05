context("test-functions.R")
setwd("../../../")
source("analyses/model.R")

test_that("get_trailing_table works with a simple table", {
  df = data.frame(
    date=c("2001-01-29", "2001-01-30", "2001-01-31"),
    bin=c(1, 2, 3),
    stringsAsFactors=FALSE
  )
  trailing_table = get_trailing_table(df, "2001-01-31", 2)
  expect_equal(trailing_table$date, c("2001-01-29", "2001-01-30"))
  expect_equal(trailing_table$bin, c(1, 2))
})

