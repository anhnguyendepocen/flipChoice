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

# Automated test cases
experiment <- CreateExperiment(c(3, 5, 7, 10), 20)
experiment2 <- CreateExperiment(c(4, 4, 4, 4), 0)


# TODO add tests for equality with specific designs
tfile <- tempfile()
withr::with_output_sink(tfile, {
    for (model in c("Random", "Complete enumeration" )) {  # can't handle prohibitions with "Shortcut"
        for (output in c("Attributes and levels", "Prohibitions", "Unlabeled design", "Labeled design",
                         "Balances and overlaps", "Standard errors")) {

                test_that(paste(model, output), {
                    cmd <- ChoiceModelDesign(design.algorithm = model,
                                             attribute.levels = experiment$attribute.levels,
                                             prior = NULL,
                                             n.questions = 10,
                                             n.versions = 2,
                                             alternatives.per.question = 3,
                                             prohibitions = experiment$prohibitions,
                                             none.alternatives = 0,
                                             labeled.alternatives = FALSE,
                                             output = output)
                    expect_error(print(cmd), NA)
                    cmd <- ChoiceModelDesign(design.algorithm = model,
                                             attribute.levels = attribute.levels,
                                             prior = NULL,
                                             n.questions = 12,
                                             n.versions = 2,
                                             alternatives.per.question = 3,
                                             prohibitions = prohibitions,
                                             none.alternatives = 2,
                                             labeled.alternatives = TRUE,
                                             output = output)
                    expect_error(print(cmd), NA)
                    cmd <- ChoiceModelDesign(design.algorithm = model,
                                             attribute.levels = experiment2$attribute.levels,
                                             prior = NULL,
                                             n.questions = 6,
                                             n.versions = 6,
                                             alternatives.per.question = 5,
                                             prohibitions = experiment2$prohibitions,
                                             none.alternatives = 0,
                                             labeled.alternatives = FALSE,
                                             output = output)
                    expect_error(print(cmd), NA)
                })

      }
  }
})
unlink(tfile)
