context("Hierarchical Bayes")

# load("sawtoothdata.RData")
load("eggsdata.RData")

test_that("HB", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.439473684210526)
})

test_that("HB cross validation", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                             tasks.left.out = 2, hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$out.sample.accuracy, 0.396052631578947)
})

test_that("HB filter", {
    sub <- rep(FALSE, nrow(eggs.data))
    sub[1:100] <- TRUE
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                             subset = sub, hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.48375)
})

test_that("HB weights", {
    wgt <- 1:length(nrow(eggs.data))
    expect_error(FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                                weights = wgt, hb.warnings = FALSE),
                 "Weights are not able to be applied for Hierarchical Bayes.")
})

test_that("HB 2 classes", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1, n.classes = 2)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.439144736842105)
})

test_that("HB diagonal", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                             normal.covariance = "Diagonal",
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.437828947368421)
})

test_that("HB diagonal 2 classes", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                             normal.covariance = "Diagonal", n.classes = 2,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.459868421052632)
})

test_that("HB spherical", {
    result <- FitChoiceModel(eggs.data, hb.iterations = 10, hb.chains = 1,
                            normal.covariance = "Spherical",
                            hb.warnings = FALSE)
    expect_error(print(result), NA)
    # expect_equal(result$in.sample.accuracy, 0.429276315789474)
})
