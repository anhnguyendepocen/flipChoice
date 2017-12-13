# context("Data formats")
#
# library(readxl)
#
# cho.file <- "Training.cho"
# sawtooth.design.file <- "Education_Design_(Sawtooth_format).xlsx"
# jmp.design.file <- "Choice_Profiles_(JMP).xlsx"
# jmp.levels.design.file <- "Psuedo-JMP_Design_3.xlsx"
# attribute.levels.file.cho <- "Attribute_labels_-_Training.xlsx"
# attribute.levels.file.dual <- "Education_Design_Labels.xlsx"
# attribute.levels.file.jmp <- "Labels_for_Choice_Profiles.xlsx"
# sawtooth.design.rdata <- "dualfile.RData"
# jmp.rdata <- "jmp.RData"
# jmp.levels.rdata <- "jmplevels.RData"
# test.path <- "tests/testthat/"
#
# if (!dir.exists("exec")) {
#     cho.file <- paste0(test.path, cho.file)
#     sawtooth.design.file <- paste0(test.path, sawtooth.design.file)
#     jmp.design.file <- paste0(test.path, jmp.design.file)
#     jmp.levels.design.file <- paste0(test.path, jmp.levels.design.file)
#     attribute.levels.file.cho <- paste0(test.path, attribute.levels.file.cho)
#     attribute.levels.file.dual <- paste0(test.path, attribute.levels.file.dual)
#     attribute.levels.file.jmp <- paste0(test.path, attribute.levels.file.jmp)
#     sawtooth.design.rdata <- paste0(test.path, sawtooth.design.rdata)
#     jmp.rdata <- paste0(test.path, jmp.rdata)
#     jmp.levels.rdata <- paste0(test.path, jmp.levels.rdata)
# }
#
# load(sawtooth.design.rdata)
# load(jmp.rdata)
# load(jmp.levels.rdata)
#
# test_that("cho file", {
#     result <- FitChoiceModel(cho.file = cho.file,
#                              attribute.levels.file = attribute.levels.file.cho,
#                              hb.iterations = 10, hb.chains = 1,
#                              hb.warnings = FALSE)
#     expect_error(print(result), NA)
# })
#
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
#     result <- FitChoiceModel(design.file = jmp.levels.design.file,
#                              choices = choices.jmp.levels,
#                              questions = tasks.jmp.levels,
#                              hb.iterations = 10, hb.chains = 1,
#                              hb.warnings = FALSE)
#     expect_error(print(result), NA)
# })
