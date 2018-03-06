data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> S; // Number of scenarios per respondent
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma;
    row_vector[V] theta;
    cholesky_factor_corr[V] L_omega;
    matrix[V,R] standard_normal;
}

transformed parameters {
    matrix[V, V] L_sigma;
    vector[C] XB[R, S];
    matrix[R,V] beta;

    L_sigma = diag_pre_multiply(sigma, L_omega);
    beta = (L_sigma * standard_normal)';
    for (r in 1:R)
    {
       beta[r] += theta;
        for (s in 1:S)
            XB[r, s] = X[r, s] * to_vector(beta[r]);
    }
}

model {
    // setting priors

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma ~ gamma(1.39435729464721, 0.39435729464721);

    theta ~ normal(prior_mean, prior_sd);
    L_omega ~ lkj_corr_cholesky(4);

    to_vector(standard_normal) ~ normal(0, 1);

    //likelihood
    for (r in 1:R) {
        for (s in 1:S) {
            Y[r, s] ~ categorical_logit(XB[r, s]);
        }
    }
}
