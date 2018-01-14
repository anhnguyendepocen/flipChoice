context("Data formats")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipChoice", mustWork = TRUE),
              file)
}

cho.file <- findInstDirFile("Training.cho")
sawtooth.design.file <- findInstDirFile("Education_Design_(Sawtooth_format).xlsx")
jmp.design.file <- findInstDirFile("Choice_Profiles_(JMP).xlsx")
jmp.levels.design.file <- findInstDirFile("Psuedo-JMP_Design_3.xlsx")
attribute.levels.file.cho <- findInstDirFile("Attribute_labels_-_Training.xlsx")
attribute.levels.file.dual <- findInstDirFile("Education_Design_Labels.xlsx")
attribute.levels.file.jmp <- findInstDirFile("Labels_for_Choice_Profiles.xlsx")

data(sawtooth, package = "flipChoice")
data(jmp, package = "flipChoice")
data(jmplevels, package = "flipChoice")

test_that("cho file", {
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels.file = attribute.levels.file.cho,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})

# test_that("dual file format", {
#     result <- FitChoiceModel(design.file = sawtooth.design.file,
#                              attribute.levels.file = attribute.levels.file.dual,
#                              choices = choices, questions = tasks,
#                              hb.iterations = 10, hb.chains = 1,
#                              hb.warnings = FALSE)
#     expect_error(print(result), NA)
# })
#
# test_that("jmp format", {
#     result <- FitChoiceModel(design.file = jmp.design.file,
#                              attribute.levels.file = attribute.levels.file.jmp,
#                              choices = choices.jmp, questions = tasks.jmp,
#                              hb.iterations = 10, hb.chains = 1,
#                              hb.warnings = FALSE)
#     expect_error(print(result), NA)
# })
#
# test_that("jmp labels format", {
#     expect_warning(result <- FitChoiceModel(design.file = jmp.levels.design.file,
#                              choices = choices.jmp.levels,
#                              questions = tasks.jmp.levels,
#                              hb.iterations = 10, hb.chains = 1,
#                              hb.warnings = FALSE),
#         "9 respondents with missing data were omitted from the analysis")
#     expect_error(print(result), NA)
# })
