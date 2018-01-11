context("Experimental design")

# Manual test case
attributes <- c("brand", "engine", "transmission", "colour")
attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                         c("125cc", "250cc", "500cc"),
                         c("manual", "automatic"),
                         c("red", "green", "blue", "yellow", "black", "white", "silver"))
prohibitions <- matrix(c("ducati", "125cc", "manual", "red", "ducati", "", "manual", "red", "honda", "All", "", "yellow"),
                       ncol = 4, byrow = TRUE)
n.questions <- 30
alternatives.per.question <- 3

# Automated test case
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)

# TODO convert below to loop and test the different outputs


for (model in c("Random", "Shortcut", "Complete enumeration")) {
    for (output in c("Attributes and levels", "Prohibitions", "Unlabelled design", "Labelled design",
                     "Level balances")) {

        test_that(paste(model, output), {
            expect_error(ChoiceModelDesign(model, attribute.levels, n.questions,
                                           alternatives.per.question, prohibitions, output), NA)
            expect_error(ChoiceModelDesign(model, experiment$attribute.levels, n.questions,
                                           alternatives.per.question, experiment$prohibitions, output), NA)
        })
    }
}
