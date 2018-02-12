data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> S; // Number of scenarios per respondent
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of variables
    int<lower=1> V_attribute[A]; // Number of variables in each attribute
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma;
    vector[V] theta;
    cholesky_factor_corr[V] L_omega;
    vector[V] standard_normal[R];
}

transformed parameters {
    matrix[V, V] L_sigma;
    vector[C] XB[R, S];
    vector[V] beta[R];

    L_sigma = diag_pre_multiply(sigma, L_omega);

    for (r in 1:R)
    {
        beta[r] = theta + L_sigma * standard_normal[r];
        for (s in 1:S)
            XB[r, s] = X[r, s] * beta[r];
    }
}

model {
    // setting priors

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma ~ gamma(1.39435729464721, 0.39435729464721);

    for (v in 1:V)
        theta[v] ~ normal(prior_mean[v], prior_sd[v]);
    L_omega ~ lkj_corr_cholesky(4);

    for (r in 1:R)
        standard_normal[r] ~ normal(0, 1);

    //likelihood
    for (r in 1:R) {
        for (s in 1:S) {
            Y[r, s] ~ categorical_logit(XB[r, s]);
        }
    }
}
