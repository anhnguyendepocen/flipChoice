context("Hierarchical Bayes")

data(eggs, package = "flipChoice")

test_that("HB", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1, hb.warnings = FALSE,
                             hb.beta.draws.to.keep = 2)
    expect_error(print(result), NA)
    expect_equal(dim(result$beta.draws), c(2L, 380L, 13L))
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

    ExtractParameterStats(result)
    PlotPosteriorIntervals(result)
    TracePlots(result)
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

test_that("HB prior attributes", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1,
                             hb.prior.mean = c(0, -10, 0, 0, 0, 0, 0, 20),
                             hb.prior.sd = rep(0.1, 8), hb.warnings = FALSE)
    expect_error(print(result), NA)
    # Weight variable prior forced to be negative
    expect_equal(all(result$parameter.statistics[3:5, 1] < 0), TRUE)
    # Price variable prior forced to be positive
    expect_equal(result$parameter.statistics[13, 1] > 0, TRUE)
})

test_that("HB prior variables", {
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 10,
                             hb.chains = 1,
                             hb.prior.mean = c(0, 0, 0, 0, 0, 0, 0, 0, -10, 0,
                                               0, 0, 0),
                             hb.prior.sd = rep(0.1, 8), hb.warnings = FALSE)
    expect_error(print(result), NA)
    # Free range variable prior force to be negative
    expect_equal(result$parameter.statistics[9, 1] < 0, TRUE)
})
