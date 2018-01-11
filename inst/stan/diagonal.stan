functions {
    vector completeTheta(vector theta_raw, int A, int V, int[] V_attribute, vector prior_mean)
    {
        vector[V] theta;
        int variable_i = 1;
        int raw_variable_i = 1;
        for (i in 1:A)
        {
            if (V_attribute[i] > 1)
            {
                int v = V_attribute[i];
                for (j in 2:v)
                    theta[variable_i + j - 1] = theta_raw[raw_variable_i + j - 2];

                if (prior_mean[raw_variable_i] == 0)
                    theta[variable_i] = -sum(theta_raw[raw_variable_i:(raw_variable_i + v - 2)]);
                else
                    theta[variable_i] = 0;

                variable_i = variable_i + v;
                raw_variable_i = raw_variable_i + v - 1;
            }
            else
            {
                theta[variable_i] = theta_raw[raw_variable_i];
                variable_i = variable_i + 1;
                raw_variable_i = raw_variable_i + 1;
            }
        }
        return theta;
    }
}

data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> S; // Number of scenarios per respondent
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of variables
    int<lower=1> V_raw; // Number of raw variables
    int<lower=1> V_attribute[A]; // Number of variables in each attribute
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs
    int<lower=1> U; // Number of standard deviation parameters
    vector[V_raw] prior_mean; // Prior mean for theta_raw
    vector[V_raw] prior_sd; // Prior sd for theta_raw
}

parameters {
    vector[V_raw] theta_raw;
    cholesky_factor_corr[V] L_omega;
    vector<lower=0>[U] sigma_unique;
    vector[V] standard_normal[R];
}

transformed parameters {
    vector[V] theta;
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

    theta = completeTheta(theta_raw, A, V, V_attribute, prior_mean);

    for (r in 1:R)
    {
        beta[r] = theta + sigma .* standard_normal[r];
        for (s in 1:S)
            XB[r, s] = X[r, s] * beta[r];
    }
}

model {
    //priors

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma_unique ~ gamma(1.39435729464721, 0.39435729464721);

    for (v in 1:V_raw)
        theta_raw[v] ~ normal(prior_mean[v], prior_sd[v]);

    for (r in 1:R)
        standard_normal[r] ~ normal(0, 1);

    //likelihood
    for (r in 1:R) {
        for (s in 1:S) {
            Y[r, s] ~ categorical_logit(XB[r, s]);
        }
    }
}
