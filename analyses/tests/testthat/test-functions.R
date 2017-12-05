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

test_that("get_trailing_table works with a simple table", {
  df = data.frame(
    date=c("2001-01-31", "2001-01-31", "2001-01-31"),
    bin=c(1, 2, 3),
    stringsAsFactors=FALSE
  )
  trailing_table = get_trailing_table(df, "2001-01-31", 2)
  
  expect_equal(trailing_table$date, character(0))
  expect_equal(trailing_table$bin, numeric(0))
})

test_that("get_bin_score works with a simple table", {
  df = data.frame(
    date=c("2001-01-29", "2001-01-30", "2001-01-31"),
    bin=c(1, 1, 1),
    num_crimes=c(3, 2, 1),
    stringsAsFactors=FALSE
  )
  
  r = 5
  score = get_bin_score(1, df, "2001-01-31", r)
  expect_equal(exp(-r * 2) * 3 + exp(-r * 1) * 2 + 1, score)
})

test_that("get_bin_scores works with a simple table", {
  df = data.frame(
    date=rep(c("2001-01-29", "2001-01-30", "2001-01-31"), 3),
    bin=c(rep(1, 3), rep(2, 3), rep(3, 3)),
    num_crimes=c(rep(3, 3), rep(2, 3), rep(1, 3)),
    stringsAsFactors=FALSE
  )
  
  date = "2001-01-31"
  r = 5
  result = get_bin_scores(df, date, r)
  
  expect_equal(result$bin[1:3], c(1, 2, 3))
  expect_equal(result$bin_score[1:3], c(
    get_bin_score(1, df, date, r),
    get_bin_score(2, df, date, r),
    get_bin_score(3, df, date, r)
  ))
})

test_that("get_neighbor_adjusted_bin_score works with a simple table", {
  df = data.frame(
    bin=c(1, 2, 3, 4, 5),
    bin_scores=c(0.1, 0.2, 0.3, 0.4, 0.5)
  )
  touching_dict = list(c(2, 3, 4, 5))
  
  s = 0.75
  final_score = get_neighbor_adjusted_bin_score(1, touching_dict, df, s)
  expect_equal(final_score, (0.2 + 0.3 + 0.4 + 0.5) * s + 0.1 * (1 - s))
})

test_that("get_maximal_capture works with a simple table", {
  df = data.frame(
    bin=c(1, 2, 3, 4, 5),
    num_crimes=c(1, 2, 3, 4, 5),
    date=rep("2001-01-31", 5)
  )
  
  date = "2001-01-31"
  k = 3
  capture = get_maximal_capture(df, date, k)
  expect_equal(capture, (5 + 4 + 3) / (5 + 4 + 3 + 2 + 1))
})

test_that("get_predicted_bins works with a simple table", {
  df = data.frame(
    date=rep("2001-01-30", 5),
    bin=c(1, 2, 3, 4, 5), 
    num_crimes=c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  
  touching_dict = list(
    c(2, 3, 4, 5),
    c(1, 3, 4, 5),
    c(1, 2, 4, 5),
    c(1, 2, 3, 5),
    c(1, 2, 3, 4)
  )
  for (i in 6:661) {
    touching_dict[[i]] = numeric(0)
  }
  k = 3
  n = 1
  r = 5
  s = 0.75
  predicted_bins = get_predicted_bins(df, touching_dict, "2001-01-31", k, n, r, s)
  expect_equal(predicted_bins, c(1, 2, 3))
})