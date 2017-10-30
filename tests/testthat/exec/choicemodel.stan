functions {
    vector completeTheta(vector theta_raw, int A, int V, int[] V_attribute)
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
                theta[variable_i] = -sum(theta_raw[raw_variable_i:(raw_variable_i + v - 2)]);
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
}

parameters {
    vector[V_raw] theta_raw;
    cholesky_factor_corr[V] L_omega;
    vector<lower=0, upper=pi()/2>[V] sigma_unif;
    vector[V] standard_normal[R];
}

transformed parameters {
    vector[V] theta;
    vector<lower=0>[V] sigma;
    matrix[V, V] L_sigma;
    vector[C] XB[R, S];
    vector[V] beta[R];

    theta = completeTheta(theta_raw, A, V, V_attribute);

    sigma = 2.5 * tan(sigma_unif);
    L_sigma = diag_pre_multiply(sigma, L_omega);

    for (r in 1:R)
    {
        beta[r] = theta + L_sigma * standard_normal[r];
        for (s in 1:S)
            XB[r, s] = X[r, s] * beta[r];
    }
}

model {
    //priors
    theta_raw ~ normal(0, 10);
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
