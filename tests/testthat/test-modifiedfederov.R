context("Modified Federov")

test_that("3*3*2/4/10 dummy coding",
{
    seed <- 3000
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- modifiedFederovDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    out <- ChoiceModelDesign(design.algorithm = "Modified Federov",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    prior <- matrix("0", nrow = 5, ncol =1)
    out2 <- ChoiceModelDesign(design.algorithm = "Modified Federov",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    expect_identical(out, out2)
    expect_equal(out$Derror, .52, tolerance = .0015)
    expect_true(all(out$mode.matrix %in% c(0, 1)))
    expect_equal(names(out$design)[-2:-1], pa[1, ])
    expect_equal(levels(out$design[[3]]), pa[-1, 1])
    expect_equal(unique(out$design[[1]]), 1:n.q)
    expect_equal(unique(out$design[[2]]), 1:apq)

    ## expect_true(all(grepl("^set[0-9]{1,2}[.]alt[1-4]", rownames(out$design))))
})

test_that("3^3/3/9 effects coding",
{
    seed <- 101
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", "car"))
    prior <- matrix(nrow = 0, ncol = 0)
    al <- pastedAttributesToVector(pa)
    out <- modifiedFederovDesign(al, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_true(all(out$model.matrix %in% c(-1, 0, 1)))
})

test_that("ModifiedFederov: bad prior",
{
    seed <- 331
    pa <- cbind(c("price", "1", "2", "", ""),
                c("time", "morn", "aft", "eve", ""),
                c("type", "train", "bus", "boat", "car"))
    al <- pastedAttributesToVector(pa)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix("1", nrow = 3, ncol = 1)
    expect_error(modifiedFederovDesign(al, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed), sQuote(n.coef))

    prior <- matrix("1", nrow = 6, ncol = 3)
    expect_error(modifiedFederovDesign(al, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed), "either one or two columns.")
})

test_that("ModifiedFederov: vector prior",
{
    seed <- 2218789
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""),
                c("type", "train", "bus", "boat", "car", "bike"),
                c("food", "candy", "sandwich", "nuts", "", ""))
    al <- pastedAttributesToVector(pa)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix("1", nrow = n.coef, ncol = 1)
    out <- modifiedFederovDesign(al, prior, 5, 15,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_equal(out$error, .325, tolerance = .05)
})

test_that("ModifiedFederov: prior means and variances",
{
    seed <- 97
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix(c("0", "2"), nrow = n.coef, ncol = 2, byrow = TRUE)
    out <- ChoiceModelDesign(design.algorithm = "Modified Federov", attribute.levels = pa,
                             prior = prior, n.questions = 8, alternatives.per.question = 3,
                                       seed = seed)
    expect_equal(out$Derror, 1.871, tolerance = 1e-3)
})

test_that("Federov: none alternatives",
{
    seed <- 20
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- modifiedFederovDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    n.a <- 2
    out <- ChoiceModelDesign(design.algorithm = "Modified Federov",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             none.alternatives = 2)
    expect_equal(sum(is.na(out$design.with.none[[3]])), n.q*n.a)
    expect_equal(max(out$design.with.none[[2L]]), n.a + apq)
})
