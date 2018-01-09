context("Experimental design")

# Manual test case
attributes <- c("brand", "engine", "transmission", "colour")
attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                         c("125cc", "250cc", "500cc"),
                         c("manual", "automatic"),
                         c("red", "green", "blue", "yellow", "black", "white", "silver"))
prohibitions <- list(c("ducati", "125cc", "manual", "red"), c("ducati", "250cc", "manual", "red"))
n.questions <- 100
alternatives.per.question <- 3

# Automated test case
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)


test_that("Random", {
    expect_error(ChoiceModelDesign("random", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("random", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})

test_that("Shortcut", {
    expect_error(ChoiceModelDesign("shortcut", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("shortcut", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})

test_that("Enumerated", {
    expect_error(ChoiceModelDesign("enumerated", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("enumerated", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})


