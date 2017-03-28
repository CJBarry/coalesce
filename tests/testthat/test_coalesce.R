library(coalesce)
library(data.table)
context("Test coalesce functionality")

Tm <- function(xyzm) sum(xyzm$m, na.rm = TRUE)
com <- function(xyzm){
  if("z" %in% names(xyzm)){
    xyzm[, lapply(list(x = x, y = y, z = z),
                  weighted.mean, m, na.rm = TRUE)]
  }else{
    xyzm[, lapply(list(x = x, y = y), weighted.mean, m, na.rm = TRUE)]
  }
}

expect_coal <- function(new, old){
  expect_equal(ncol(new), ncol(old))
  expect_lte(nrow(new), nrow(old))
  expect_equal(Tm(new), Tm(old), tolerance = 1e-5)
  expect_equal(com(new), com(old), tolerance = 1e-5)
}

test_that("check null behaviour", {
  # note that rows may still be removed where m = 0

  # 2D small
  tps <- test.points(100L, TRUE)
  expect_silent(cps <- coalesce(tps, 0, mm = 0, maxnp = Inf, TwoD = TRUE))
  expect_coal(cps, tps)

  # 3D large
  tps <- test.points(25000L, FALSE)
  expect_silent(cps <- coalesce(tps, 0, mm = 0, maxnp = Inf, TwoD = FALSE))
  expect_coal(cps, tps)
})

test_that("check conservation of mass and centre of mass", {
  # small data sets
  for(trial in 1:10){
    # 2D
    expect_silent(tps <- test.points(100L, TRUE))
    expect_silent(cps <- coalesce(tps, sample(1:20, 1), TwoD = TRUE))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, 0, mm = sample(seq(0, .5, .1), 1L),
                                  TwoD = TRUE))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, sample(1:20, 1L),
                    mm = sample(seq(0, .5, .1), 1L), TwoD = TRUE))
    expect_coal(cps, tps)

    # 3D
    expect_silent(tps <- test.points(100L, FALSE))
    expect_silent(cps <- coalesce(tps, sample(1:20, 1)))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, 0, mm = sample(seq(0, .5, .1), 1L)))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, sample(1:20, 1L),
                    mm = sample(seq(0, .5, .1), 1L)))
    expect_silent(expect_coal(cps, tps))
  }

  # large data sets
  for(trial in 1:10){
    # 2D
    expect_silent(tps <- test.points(25000L, TRUE))
    expect_silent(cps <- coalesce(tps, sample(1:20, 1), TwoD = TRUE))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, 0, mm = sample(seq(0, .5, .1), 1L),
                                  TwoD = TRUE))
    expect_coal(cps, tps)
    # expect_silent(cps <- coalesce(tps, sample(1:20, 1L),
    #                               mm = sample(seq(0, .5, .1), 1L),
    #                               TwoD = TRUE))
    # expect_coal(cps, tps)
    # expect_silent(cps <- coalesce(tps, sample(1:20, 1),
    #                               maxnp = sample(1:20, 1L), TwoD = TRUE))
    # expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, sample(1:20, 1),
                                  subregions = FALSE, TwoD = TRUE))
    expect_coal(cps, tps)

    # 3D
    expect_silent(tps <- test.points(25000L, FALSE))
    expect_silent(cps <- coalesce(tps, sample(1:20, 1)))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, 0, mm = sample(seq(0, .5, .1), 1L)))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, sample(1:20, 1L),
                                  mm = sample(seq(0, .5, .1), 1L)))
    expect_coal(cps, tps)
    expect_silent(cps <- coalesce(tps, sample(1:20, 1),
                                  subregions = FALSE))
    expect_coal(cps, tps)
  }
})
