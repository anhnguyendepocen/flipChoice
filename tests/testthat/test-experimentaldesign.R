context("Experimental design")

# Manual test case
attributes <- c("brand", "engine", "transmission", "colour")
attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                         c("125cc", "250cc", "500cc"),
                         c("manual", "automatic"),
                         c("red", "green", "blue", "yellow", "black", "white", "silver"))
prohibitions <- matrix(c("ducati", "125cc", "manual", "red", "ducati", "", "manual", "red", "honda", "All", "", "yellow"),
                       ncol = 4, byrow = TRUE)
n.questions <- 100
alternatives.per.question <- 3

# Automated test case
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)

# TODO convert below to loop

test_that("Random", {
    expect_error(ChoiceModelDesign("Random", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("Random", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})

test_that("Shortcut", {
    expect_error(ChoiceModelDesign("Shortcut", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("Shortcut", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})

test_that("Enumerated", {
    expect_error(ChoiceModelDesign("Complete enumeration", attribute.levels, n.questions,
                                   alternatives.per.question, prohibitions), NA)
    expect_error(ChoiceModelDesign("Complete enumeration", experiment$attribute.levels, n.questions,
                                   alternatives.per.question, experiment$prohibitions), NA)
})


