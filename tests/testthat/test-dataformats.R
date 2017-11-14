context("Data formats")

library(readxl)

cho.file <- if (!dir.exists("exec")) {
    "tests/testthat/Training.cho"
} else {
    "Training.cho"
}

raw.attribute.levels <- if (!dir.exists("exec"))
{
    read_excel("tests/testthat/Attribute labels - Training.xlsx")
} else {
    read_excel("Attribute labels - Training.xlsx")
}

test_that("cho file", {
    result <- FitChoiceModel(cho.file = cho.file,
                             attribute.levels = raw.attribute.levels,
                             hb.iterations = 10, hb.chains = 1,
                             hb.warnings = FALSE)
    expect_error(print(result), NA)
})
