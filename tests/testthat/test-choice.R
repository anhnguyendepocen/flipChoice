context("Hierarchical Bayes")

# load("tests/testthat/sawtoothdata.RData")
# load("tests/testthat/eggsdata.RData")
load("eggsdata.RData")

test_that("HB", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE,
                             hb.max.draws = 2)
    expect_error(print(result), NA)
    expect_equal(dim(result$beta.draws), c(2L, 380L, 20L))
})

test_that("HB cross validation", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, tasks.left.out = 2,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB filter", {
    sub <- rep(FALSE, nrow(eggs.data))
    sub[1:100] <- TRUE
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, subset = sub, hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB weights", {
    wgt <- 1:length(nrow(eggs.data))
    expect_error(FitChoiceModel(experiment.data = eggs.data,
                                hb.iterations = 10, hb.chains = 1,
                                weights = wgt, hb.warnings = FALSE),
                 "Weights are not able to be applied for Hierarchical Bayes.")
})

test_that("HB 2 classes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, n.classes = 2, hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB diagonal", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, normal.covariance = "Diagonal",
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB diagonal 2 classes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, normal.covariance = "Diagonal",
                             n.classes = 2, hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB spherical", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, normal.covariance = "Spherical",
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("HB constraints", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1,
                             hb.prior.mean = c(0, 1, 0, 0, 1, 0, 0, -2),
                             hb.prior.sd = rep(3, 8), hb.warnings = FALSE)
    expect_error(print(result), NA)
})
