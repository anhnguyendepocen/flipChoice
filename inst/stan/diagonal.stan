data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> S; // Number of scenarios per respondent
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs
    int<lower=1> U; // Number of standard deviation parameters
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector[V] theta;
    vector<lower=0>[U] sigma_unique;
    vector[V] standard_normal[R];
}

transformed parameters {
    vector<lower=0>[V] sigma;
    vector[C] XB[R, S];
    vector[V] beta[R];

    if (U == 1)
    {
        for (v in 1:V)
            sigma[v] = sigma_unique[1];
    }
    else
        sigma = sigma_unique;

    for (r in 1:R)
    {
        beta[r] = theta + sigma .* standard_normal[r];
        for (s in 1:S)
            XB[r, s] = X[r, s] * beta[r];
    }
}

model {
    // setting priors

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma_unique ~ gamma(1.39435729464721, 0.39435729464721);

    for (v in 1:V)
        theta[v] ~ normal(prior_mean[v], prior_sd[v]);

    for (r in 1:R)
        standard_normal[r] ~ normal(0, 1);

    //likelihood
    for (r in 1:R) {
        for (s in 1:S) {
            Y[r, s] ~ categorical_logit(XB[r, s]);
        }
    }
}
