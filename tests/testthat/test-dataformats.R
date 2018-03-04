context("Data formats")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipChoice", mustWork = TRUE),
              file)
}

cho.file <- findInstDirFile("Training.cho")
cho.none.file <- findInstDirFile("none_option.cho")
jmp.design.file <- findInstDirFile("eggs_design.xlsx")
jmp.design.with.levels.file <- findInstDirFile("eggs_design_with_levels.xlsx")

attribute.levels.file.cho <- findInstDirFile("Attribute_labels_-_Training.xlsx")
attribute.levels.file.jmp <- findInstDirFile("eggs_labels.xlsx")

data(eggs, package = "flipChoice")

choices.jmp <- eggs.data[, 1:8]
choices.jmp.none.of.these <- choices.jmp
for (i in 1:8)
{
    v <- as.numeric(choices.jmp.none.of.these[[i]])
    v[1] <- 4
    choices.jmp.none.of.these[[i]] <- as.factor(v)
    levels(choices.jmp.none.of.these[[i]]) <- LETTERS[1:4]
}
tasks.jmp <- data.frame(t(matrix(1:3040, nrow = 8)))

cho.file <- findInstDirFile("Training.cho")
cho.none.file <- findInstDirFile("none_option.cho")
sawtooth.design.file <- findInstDirFile("Education_Design_(Sawtooth_format).xlsx")
jmp.design.file <- findInstDirFile("Choice_Profiles_(JMP).xlsx")
jmp.levels.design.file <- findInstDirFile("Psuedo-JMP_Design_3.xlsx")
attribute.levels.file.cho <- findInstDirFile("Attribute_labels_-_Training.xlsx")
attribute.levels.file.dual <- findInstDirFile("Education_Design_Labels.xlsx")
attribute.levels.file.jmp <- findInstDirFile("Labels_for_Choice_Profiles.xlsx")

data(sawtooth, package = "flipChoice")
data(jmp, package = "flipChoice")
data(jmplevels, package = "flipChoice")
data(eggs, package = "flipChoice")

test_that("cho file", {
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("cho none file", {
    result <- FitChoiceModel(cho.file = cho.none.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format none of these", {
    result <- FitChoiceModel(design.file = jmp.design.file,
                             attribute.levels.file = attribute.levels.file.jmp,
                             choices = choices.jmp.none.of.these,
                             questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("jmp format with labels", {
    result <- FitChoiceModel(design.file = jmp.design.with.levels.file,
                             choices = choices.jmp, questions = tasks.jmp,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

test_that("Missing data", {
    eggs.data.missing <- eggs.data
    eggs.data.missing[1, 1] <- NA
    dat <- processExperimentData(experiment.data = eggs.data.missing,
                                 subset = NULL, weights = NULL,
                                 n.questions.left.out = 0,
                                 seed = 123, input.prior.mean = 0,
                                 input.prior.sd = 5)
    expect_equal(dat$n.respondents, 379)
    expect_equal(dim(dat$X.in), c(379L, 8L, 3L, 13L))
    expect_equal(dim(dat$Y.in), c(379L, 8L))
})
