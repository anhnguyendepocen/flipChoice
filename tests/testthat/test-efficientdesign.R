context("Efficient")

test_that("3*3*2/4/10 dummy coding; old interface",
{
    seed <- 3000
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- NULL  # matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    prior <- numeric(5)
    out2 <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = prior, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed)
    expect_identical(out, out2)
    expect_equal(out$db.error, .52, tolerance = .0015)
    expect_true(all(out$model.matrix %in% c(0, 1)))
    expect_equal(colnames(out$design)[-3:-1], pa[1, ])
    expect_equal(unique(out$design[, 2]), 1:n.q)
    expect_equal(unique(out$design[, 3]), 1:apq)

    ## expect_true(all(grepl("^set[0-9]{1,2}[.]alt[1-4]", rownames(out$design))))
})

seed <- 765
pd <- cbind(c("price", "200", "250", "300"), c("Mean", 0, 1, 1),
            c("SD", 1, 1, 1),
            c("time", "morn", "aft", "eve"), c("Mean", 0, 0, 0),
            c("SD", 1, 1, 1),
            c("type", "train", "bus", ""), c("Mean", 2, 3, ""), c("SD", 1, 2, ""))
vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
n.q <- 10
apq <- 4

out <- ChoiceModelDesign(design.algorithm = "Efficient",
                         attribute.levels = pd, prior = NULL, n.questions = n.q,
                         alternatives.per.question = apq, seed = seed,
                         output = "Labeled design")

test_that("Some prior inputs missing",
{

    expect_equal(out$db.error, .945, tolerance = .01)
    expect_true(all(out$model.matrix %in% c(0, 1)))
    expect_equal(colnames(out$design)[-3:-1], vnames)
    expect_equal(unique(out$design[, 2]), 1:n.q)
    expect_equal(unique(out$design[, 3]), 1:apq)

     ## expect_true(all(grepl("^set[0-9]{1,2}[.]alt[1-4]", rownames(out$design))))
})

test_that("ChoiceModelDesign: print labels working",
{

    tfile <- tempfile()
    withr::with_output_sink(tfile, {
        expect_is(print(out), "data.frame")
        expect_equal(levels(print(out)[[4]]), pd[-1, 1])
        expect_equal(levels(print(out)[[5]]),
                     pd[-1, pd[1,] == colnames(print(out))[5]])
        expect_named(print(out), c("Version", "Question", "Alternative", "price", "time", "type"))
    })
    unlink(tfile)
})


test_that("3^3/3/9 effects coding",
{
    seed <- 101
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", "car"))
    al <- pastedAttributesToVector(pa)
    prior <- numeric(sum(al) - length(al))
    out <- efficientDesign(al, prior, 4, 12,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_true(all(out$model.matrix %in% c(-1, 0, 1)))
})

test_that("Efficient: bad prior",
{
    seed <- 331
    pd <- cbind(c("price", "200", "250", "300"), c("mean", 0, 1, ""),
                c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""), c("mean", 2, 3, ""), c("sd", 1, 2, ""))
    expect_error(ChoiceModelDesign("Efficient", pd,
                                   alternatives.per.question = 4, n.questions = 12,
                                       seed = seed), "price, 3")

    pd <- cbind(c("price", "200", "250", "300"), c("mean", 0, 1, "2"),
                c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""), c("mean", 2, 3, ""), c("sd", 1, "", ""))
    expect_error(ChoiceModelDesign("Efficient", pd,
                                   alternatives.per.question = 4, n.questions = 12,
                                       seed = seed), "type, 2")
})

test_that("Efficient: vector prior",
{
    seed <- 2218789
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""),
                c("type", "train", "bus", "boat", "car", "bike"),
                c("food", "candy", "sandwich", "nuts", "", ""))
    al <- pastedAttributesToVector(pa)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- 1 + numeric(n.coef)
    out <- efficientDesign(al, prior, 5, 15,
                                       dummy.coding = FALSE,
                                       seed = seed)
    expect_equal(out$error, .325, tolerance = .05)
})

test_that("Efficient: prior means and variances old interface",
{
    seed <- 97
    pa <- cbind(c("price", "100", "125", "150", "175", "200"),
                c("time", "morn", "aft", "eve", "late night", ""))
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    prior <- matrix(c(0, 2), nrow = n.coef, ncol = 2, byrow = TRUE)
    out <- ChoiceModelDesign(design.algorithm = "Efficient", attribute.levels = pa,
                             prior = prior, n.questions = 8, alternatives.per.question = 3,
                                       seed = seed)
    expect_equal(out$db.error, 2.43, tolerance = 1e-3)
})

test_that("Efficient: none alternatives",
{
    seed <- 20
    pa <- cbind(c("price", "200", "250", "300"), c("time", "morn", "aft", "eve"),
                c("type", "train", "bus", ""))
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 10
    apq <- 4
    n.a <- 2
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             none.alternatives = 2)
    expect_equal(sum(is.na(out$design.with.none[, 4L])), n.q*n.a)
    expect_equal(max(out$design.with.none[, 3L]), n.a + apq)
})

test_that("Efficient: labeled alternatives",
{
    seed <- 98
    lpa1 <- c(engine = 3, transmission = 2, colour = 7)
    lpa2 <- c(brand = 4, lpa1)
    prior <- matrix(nrow = 0, ncol = 0)
    ## out <- efficientDesign(al, prior, 4, 10, dummy.coding = TRUE,
    ##                                    seed = seed)
    n.q <- 20

    out <- efficientDesign(
                                   levels.per.attribute = lpa2,
                                   prior = NULL,
                                   lpa2[1],
                                   n.q,
                                   labeled.alternatives = TRUE,
                                   dummy.coding = TRUE,
                                   seed = seed,
                                   n.sim = 10)
    expect_equal(dim(out$design), c(lpa2[1]*n.q, 2 + length(lpa2)),
                 check.attributes = FALSE)
    expect_equal(colnames(out$design), c("Question", "Alternative", names(lpa2)))
    expect_equal(dim(out$model.matrix), c(lpa2[1]*n.q,
                                          sum(lpa2) - length(lpa2)),
                 check.attributes = FALSE)

    pa <- cbind(c("yamaha", "honda", "ducati", "kawasaki", "", "", ""),
                             c("125cc", "250cc", "500cc", "", "", "", ""),
                             c("manual", "automatic", "", "", "", "", ""),
                             c("red", "green", "blue", "yellow", "black", "white", "silver"))
    pa <- rbind(c("brand", "engine", "transmission", "colour"), pa)
    out2 <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pa, prior = NULL, n.questions = n.q,
                             seed = seed,
                             labeled.alternatives = TRUE)
    n.coef <- sum(pa[-1, ] != "") - ncol(pa)
    apq <- sum(pa[-1, 1] != "")
    expect_equal(dim(out2$design), c(apq*n.q, 3 + ncol(pa)),
                 check.attributes = FALSE)
    expect_equal(colnames(out2$design), c("Version", "Question", "Alternative", pa[1, ]))
    expect_equal(dim(out2$model.matrix), c(apq*n.q,
                                          n.coef),
                 check.attributes = FALSE)
})


test_that("Parsing of pasted prior with some means and sd's missing",
{
    pd <- cbind(c("price", "200", "250", "300"), c("Mean", 0, 1, 1),
                c("time", "morn", "aft", "eve"), c("Mean", 0, 0, 2),
                c("SD", 1, 3, 1),
                c("type", "train", "bus", ""), c("SD", 1, 2, ""))
    vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
    n.q <- 10
    apq <- 4

    ## out <- ChoiceModelDesign(design.algorithm = "Efficient",
    ##                          attribute.levels = pd, prior = NULL, n.questions = n.q,
    ##                          alternatives.per.question = apq, seed = seed,
    ##                          output = "Labeled design")
    parsed <- parsePastedData(pd)
    expect_equal(names(parsed$lvls), vnames)
    expect_equal(parsed$lvls, c(3, 3, 2), check.attributes = FALSE)
    expect_equal(parsed$prior[, 1], c(1, 1, 0, 2, 0), check.attributes = FALSE)
    expect_equal(parsed$prior[, 2], c(1, 1, 3, 1, 2), check.attributes = FALSE)
})


test_that("Correct prior specification improves fit on sim data",
{
    seed <- 378
    seed.resp.sim <- 202
    n.respondents <- 15
    price.mean <- c(-2, 0, 2)
    time.mean <- 0:2
    type.mean <- 0:1
    pd <- cbind(c("price", "200", "250", "300"),
                c("Mean", price.mean),
                c("SD", c(1, 1, 1)),
                c("time", "morn", "aft", "eve"),
                c("Mean", time.mean),
                c("type", "train", "bus", ""),
                c("Mean", type.mean, ""))
    vnames <- pd[1, !pd[1,] %in% c("SD", "Mean")]
    n.q <- 10
    apq <- 4

    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pd, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    set.seed(seed.resp.sim)

    true.coef <- c(price.mean[-1], time.mean[-1], type.mean[-1])
    cfun <- contr.treatment
    mm <- model.matrix(~as.factor(price)+as.factor(time)+as.factor(type),
                       data = as.data.frame(out$design), contrasts.arg = cfun)[, -1]
    y <- as.vector(replicate(n.respondents, idefix::RespondMNL(true.coef,
                                                     mm, n.alts = apq)))
    ml.model <- mlogitModel(out, as.logical(y))
    res.good <- summary(ml.model)$CoefTable
    sd.good <- summary(ml.model)$CoefTable[, 2]
    pval.good <- summary(ml.model)$CoefTable[, 4]


    pd.bad.prior <- cbind(c("price", "200", "250", "300"),
                c("Mean", rev(price.mean)),
                c("SD", c(1, 1, 1)),
                c("time", "morn", "aft", "eve"),
                c("Mean", time.mean),
                c("SD", c(1, 1, 1)),
                c("type", "train", "bus", ""),
                c("Mean", type.mean, ""),
                c("SD", c(1, 1, "")))

    out.bad.prior <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = pd.bad.prior, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    mm <- model.matrix(~as.factor(price)+as.factor(time)+as.factor(type),
                       data = as.data.frame(out.bad.prior$design), contrasts.arg = cfun)[, -1]
    y <- as.vector(replicate(n.respondents, idefix::RespondMNL(true.coef,
                                                     mm, n.alts = apq)))
    ml.model.bad <- mlogitModel(out.bad.prior, y)
    sd.bad <- summary(ml.model.bad)$CoefTable[, 2]
    pval.bad <- summary(ml.model.bad)$CoefTable[, 4]
    summary(ml.model.bad)$CoefTable
    expect_true(sd.good["price300"] < sd.bad["price300"])
})

test_that("D-error calculation agrees with Huber Zwerina ex.",
{
    data("hz.design", package = "flipChoice")
    mm <- model.matrix(~as.factor(Attribute_1)+as.factor(Attribute_2)+as.factor(Attribute_3),
                       as.data.frame(hz.design))[, -1]
    expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, 3),
          DerrorHZ(cbind(1, hz.design), attribute.levels = c(3,3,3), TRUE))
    expect_equal(idefix:::Derr(numeric(ncol(mm)), mm, 3),
                 .192, tolerance = .0005)
})

test_that("Efficient outperforms choiceDes",
{
    seed <- 777

    levs1 <- c(3,3,5,4)
    names(levs1) <- letters[seq_along(levs1)]
    n.q <- 16
    apq <- 4

    des <- choiceDes::dcm.design(levs1, nb = 1, sets = n.q, apq)
    cd.df <- des$levels
    attr.list <- lapply(levs1, seq.int)
    out <- ChoiceModelDesign(design.algorithm = "Efficient",
                             attribute.levels = attr.list, prior = NULL, n.questions = n.q,
                             alternatives.per.question = apq, seed = seed,
                             output = "Labeled design")
    mm <- model.matrix(~as.factor(a)+as.factor(b)+as.factor(c)+as.factor(d),
                       as.data.frame(out$design))[, -1]
    expect_true(idefix:::Derr(numeric(ncol(mm)), mm, apq) <
                idefix:::Derr(numeric(ncol(mm)), as.matrix(des$effects$design), apq))
})
