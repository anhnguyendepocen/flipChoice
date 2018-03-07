context("Partial profiles")

attribute.levels <- list(Att1 = 1:2, Att2 = 1:2, Att3 = 1:2,
                         Att4 = 1:4, Att5 = 1:4)

test_that("Utility neutral partial profiles", {
    result <- ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = attribute.levels,
                                prior = NULL,
                                n.questions = 18,
                                n.versions = 1,
                                alternatives.per.question = 2,
                                n.constant.attributes = 3,
                                seed = 1)
    # Optimal criterion is 4
    expect_equal(result$d.criterion, 3.48486328125)
})
