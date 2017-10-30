context("Hierarchical Bayes")

load("sawtoothdata.RData")
load("eggsdata.RData")

test_that("HB", {
    result <- FitChoiceModel(sawtooth.data, hb.iterations = 100, hb.chains = 2)
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.91)

    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2)
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.946052631578947)
})

test_that("HB cross validation", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2, tasks.left.out = 2)
    expect_error(print(result), NA)
    expect_equal(result$out.sample.accuracy, 0.675)
})

test_that("HB filter", {
    sub <- rep(FALSE, nrow(eggs.data))
    sub[1:100] <- TRUE
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2, subset = sub)
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.9725)
})

test_that("HB weights", {
    wgt <- 1:length(nrow(eggs.data))
    expect_error(FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2, weights = wgt),
                 "Weights are not able to be applied for Hierarchical Bayes.")
})

test_that("HB 2 classes", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2, n.classes = 2)
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.952960526315789)
})

test_that("HB diagonal", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2,
                             normal.covariance = "Diagonal")
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.949013157894737)
})

test_that("HB diagonal 2 classes", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2,
                             normal.covariance = "Diagonal", n.classes = 2)
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.953618421052632)
})

test_that("HB spherical", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 100, hb.chains = 2,
                            normal.covariance = "Spherical")
    expect_error(print(result), NA)
    expect_equal(result$in.sample.accuracy, 0.956907894736842)
})

