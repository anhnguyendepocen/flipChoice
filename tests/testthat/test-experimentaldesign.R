context("Experimental design")

# Manual test case
attribute.levels <- list(c("yamaha", "honda", "ducati", "triumph", "bmw", "kawasaki"),
                         c("125cc", "250cc", "500cc"),
                         c("manual", "automatic"),
                         c("red", "green", "blue", "yellow", "black", "white", "silver"))
names(attribute.levels) <- c("brand", "engine", "transmission", "colour")
prohibitions <- matrix(c("ducati", "125cc", "manual", "red", "ducati", "", "manual", "red", "honda", "All", "", "yellow"),
                       ncol = 4, byrow = TRUE)
n.questions <- 10
n.versions <- 2
alternatives.per.question <- 3

# Automated test case
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)

# TODO add tests for equality with specific designs

for (model in c("Random", "Shortcut", "Complete enumeration")) {
    for (output in c("Attributes and levels", "Prohibitions", "Unlabelled design", "Labelled design",
                     "Balances and overlaps", "Standard errors")) {

        test_that(paste(model, output), {
            expect_error(print(ChoiceModelDesign(model, experiment$attribute.levels, n.questions, n.versions,
                                           alternatives.per.question, experiment$prohibitions,
                                           0, FALSE, output)), NA)
            expect_error(print(ChoiceModelDesign(model, attribute.levels, n.questions, n.versions,
                                           alternatives.per.question, prohibitions,
                                           2, TRUE, output)), NA)
        })
    }
}
