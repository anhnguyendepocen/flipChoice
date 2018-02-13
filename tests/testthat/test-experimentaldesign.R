context("Experimental design")

# Manual test case
manual.attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                         c("125cc", "250cc", "500cc"),
                         c("manual", "automatic"),
                         c("red", "green", "blue", "yellow", "black", "white", "silver"))
names(manual.attribute.levels) <- c("brand", "engine", "transmission", "colour")
manual.prohibitions <- matrix(c("ducati", "125cc", "manual", "red", "ducati", "", "manual", "red", "honda", "All", "", "yellow"),
                       ncol = 4, byrow = TRUE)

# Automated test cases
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)
experiment2 <- CreateExperiment(c(4, 4, 4, 4), 0)

levels.test.cases <- list(manual.attribute.levels, experiment$attribute.levels, experiment2$attribute.levels)
prohibitions.test.cases <- list(manual.prohibitions, experiment$prohibitions, experiment2$prohibitions)
questions.test.cases <- c(10, 12, 6)
versions.test.cases <- c(2, 1, 6)
alternatives.test.cases <- c(3, 3, 6)
none.test.cases <- c(0, 2, 0)
labeled.test.cases <- c(FALSE, TRUE, FALSE)

# TODO add tests for equality with specific designs
tfile <- tempfile()
withr::with_output_sink(tfile, {
    for (model in c("Random", "Complete enumeration", "Balanced overlap", "Shortcut")) {  # can't handle prohibitions with "Shortcut"
        for (output in c("Attributes and levels", "Prohibitions", "Unlabeled design", "Labeled design",
                         "Balances and overlaps", "Standard errors")) {
            for (i in seq(length(levels.test.cases))) {
                test_that(paste(model, output, "Test case", i), {
                    prohibitions <- if(model == "Shortcut") NULL else prohibitions.test.cases[[i]]
                    cmd <- ChoiceModelDesign(design.algorithm = model,
                                                 attribute.levels = levels.test.cases[[i]],
                                                 prior = NULL,
                                                 n.questions = questions.test.cases[i],
                                                 n.versions = versions.test.cases[i],
                                                 alternatives.per.question = alternatives.test.cases[i],
                                                 prohibitions = prohibitions,
                                                 none.alternatives = none.test.cases[i],
                                                 labeled.alternatives = labeled.test.cases[i],
                                                 output = output)
                    expect_error(print(cmd), NA)
                })
            }
        }
    }
})
unlink(tfile)
