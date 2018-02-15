setwd("~/flip/flipChoice")
devtools::load_all(".")

data(eggs, package = "flipChoice")

n.iter <- 500
n.sims <- 10
n.leave.out.q <- 2
n.chains <- 1
comp.stats <- array(dim = c(n.sims, 2, 9))
for (i in 1:n.sims)
{
    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                                 hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                                 hb.max.draws = 2, reduced = TRUE, seed = i+654)
    ## samps <- extract(result$stan.fit, pars = c("theta", "sigma"))
    ## samps <- do.call(cbind, samps)
    samps <- as.array(result$stan.fit)
    samps <- samps[, , grepl("theta|sigma", dimnames(samps)[[3]]), drop = FALSE]
    chain.stats <- monitor(samps, warmup = 0, probs = .5, print = FALSE)
    rhats <- chain.stats[, "Rhat"]
    neffs <- chain.stats[, "n_eff"]
    ## mean(rhats)
    ## mean(neffs)
    ## result$in.sample.accuracy
    ## result$out.sample.accuracy
    comp.stats[i, 1, ] <- c(mean.rhat = mean(rhats), mean.neff = mean(neffs),
             mean.neff.per.sec = mean(neffs)/result$time.take,
             max.rhat = max(rhats), min.neff = min(neffs),
             min.neff.per.sec = min(neffs)/result$time.take,
             in.acc = result$in.sample.accuracy,
             out.acc = result$out.sample.accuracy, time = result$time.take)

    result <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = n.iter,
                                 hb.chains = n.chains, hb.warnings = FALSE, tasks.left.out = n.leave.out.q,
                                 hb.max.draws = 2, reduced = FALSE, seed = i+654)
    samps <- as.array(result$stan.fit)
    samps <- samps[, , grepl("theta|sigma", dimnames(samps)[[3]]), drop = FALSE]
    chain.stats <- monitor(samps, warmup = 0, probs = .5, print = FALSE)
    rhats <- chain.stats[, "Rhat"]
    neffs <- chain.stats[, "n_eff"]
    comp.stats[i, 2, ] <- c(mean.rhat = mean(rhats), mean.neff = mean(neffs),
             mean.neff.per.sec = mean(neffs)/result$time.take,
             max.rhat = max(rhats), min.neff = min(neffs),
             min.neff.per.sec = min(neffs)/result$time.take,
             in.acc = result$in.sample.accuracy,
             out.acc = result$out.sample.accuracy, time = result$time.take)
}
dimnames(comp.stats)[[3]] <- c("mean.rhat", "mean.neff",
                               "mean.neff.per.sec", "max.rhat", "min.neff",
                               "min.neff.per.sec", "in.acc", "out.acc", "time")
saveRDS(comp.stats, paste0("../../Documents/Feature_ChoiceModellingDesign/eggs",
        n.iter, "sims", n.chains, "chainsComp.rds"))
colMeans(comp.stats, dim = 1)
