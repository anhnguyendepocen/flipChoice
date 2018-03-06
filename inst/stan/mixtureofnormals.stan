data {
    int<lower=2> C; // Number of alternatives (choices) in each question
    int<lower=1> R; // Number of respondents
    int<lower=1> S; // Number of questions per respondent
    int<lower=1> P; // Number of classes
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma[P];
    vector[V] theta[P];
    cholesky_factor_corr[V] L_omega[P];
    vector[V] standard_normal[R, P];
    simplex[P] class_weights;
}

transformed parameters {
    matrix[V, V] L_sigma[P];
    vector[V] class_beta[R, P];
    vector[P] posterior_prob[R];

    for (p in 1:P)
    {
        L_sigma[p] = diag_pre_multiply(sigma[p], L_omega[p]);

        for (r in 1:R)
            class_beta[r, p] = theta[p] + L_sigma[p] * standard_normal[r, p];
    }

    for (r in 1:R)
    {
        for (p in 1:P)
        {
            real prob = log(class_weights[p]);
            for (s in 1:S)
                prob = prob + categorical_logit_lpmf(Y[r, s] | X[r, s] * class_beta[r, p]);
            posterior_prob[r, p] = prob;
        }
    }
}

model {
    for (p in 1:P)
    {
        // gamma distribution with mode = 1 and p(x < 20) = 0.999
        sigma[p] ~ gamma(1.39435729464721, 0.39435729464721);

        for (v in 1:V)
            theta[p, v] ~ normal(prior_mean[v], prior_sd[v]);
        L_omega[p] ~ lkj_corr_cholesky(4);
        for (r in 1:R)
            standard_normal[r, p] ~ normal(0, 1);
    }

    //likelihood
    for (r in 1:R)
        target += log_sum_exp(posterior_prob[r]);
}

generated quantities {
    vector[V] beta[R];
    for (r in 1:R)
        for (v in 1:V)
        {
            vector[P] pp = exp(posterior_prob[r]);
            pp = pp / sum(pp);
            beta[r, v] = 0;
            for (p in 1:P)
                beta[r, v] = beta[r, v] + class_beta[r, p, v] * pp[p];
        }
}
