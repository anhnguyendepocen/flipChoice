context("Modified Fedorov")

test_that("3*3*2/4/10 dummy coding",
{
    seed <- 3000
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- matrix(nrow = 0, ncol = 0)
    out <- ModifiedFedorovChoiceDesign(pa, prior, 4, 10, dummy.coding = TRUE,
                                       seed = seed)
    prior <- matrix("0", nrow = 5, ncol =1)
    out2 <- ModifiedFedorovChoiceDesign(pa, prior, 4, 10, dummy.coding = TRUE,
                                       seed = seed)
    expect_identical(out, out2)
    expect_equal(out$error, .521094)
    expect_true(all(out$design %in% c(0, 1)))
})

test_that("3^3/3/9 effects coding",
{
    seed <- 101
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", "car"))
    prior <- matrix(nrow = 0, ncol = 0)
    out <- ModifiedFedorovChoiceDesign(pa, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_true(all(out$design %in% c(-1, 0, 1)))

})

test_that("ModifiedFedorov: bad prior",
{
    seed <- 331
    pa <- cbind(c("price", "1", "2", "", ""),
                c("time", "morn", "aft", "eve", ""),
                c("type", "train", "bus", "boat", "car"))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix("1", nrow = 3, ncol = 1)
    expect_error(ModifiedFedorovChoiceDesign(pa, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed), sQuote(n.coef))

    prior <- matrix("1", nrow = 6, ncol = 3)
    expect_error(ModifiedFedorovChoiceDesign(pa, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed), "either one or two columns.")
})

test_that("ModifiedFedorov: vector prior",
{
    seed <- 2218789
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""),
                c("type", "train", "bus", "boat", "car", "bike"),
                c("food", "candy", "sandwich", "nuts", "", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix("1", nrow = n.coef, ncol = 1)
    out <- ModifiedFedorovChoiceDesign(pa, prior, 5, 15,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_equal(out$error, .325, tolerance = .05)
})

test_that("ModifiedFedorov: prior means and variances",
{
    seed <- 97
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix(c("0", "2"), nrow = n.coef, ncol = 2, byrow = TRUE)
    out <- ModifiedFedorovChoiceDesign(pa, prior, 3, 8,
                                       dummy.coding = TRUE,
                                       seed = seed)
    expect_equal(out$error, 1.871, tolerance = 1e-3)
})
