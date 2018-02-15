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
    /* int<lower=1> V_raw; // Number of raw variables */
    /* int<lower=1> V_attribute[A]; // Number of variables in each attribute */
    int<lower=1,upper=C> Y[R, S]; // choices
    matrix[C, V] X[R, S]; // matrix of attributes for each obs **note in R this is RxSxCxV array**
    vector[V] prior_mean; // Prior mean for theta_raw
    vector[V] prior_sd; // Prior sd for theta_raw
  int<lower=1> n_versions;
  vector[R] version_idx;
  vector[n_versions,C*S] nonzero_X_idx[C*S];
}

parameters {
    vector<lower=0>[V] sigma;
    vector[V] theta;
    cholesky_factor_corr[V] L_omega;
    vector[V] standard_normal[R];
}

transformed parameters {
    /* vector[V] theta; */
    matrix[V, V] L_sigma;
    /* vector[C*S] XB[R]; */
    matrix[C*S] XB[n_versions];
    vector[V] beta[R];

    /* theta = completeTheta(theta_raw, A, V, V_attribute, prior_mean); */

    L_sigma = diag_pre_multiply(sigma, L_omega);

    for (r in 1:R)
    {
        beta[r] = theta + L_sigma * standard_normal[r];
        /* for (s in 1:S) */
        /*     XB[r, s] = X[r, s] * beta[r]; */
      for (s in 1:S)
	  XBcont[r, s] = Xcont[version_idx[r], s] * beta[r];
      for (i in 1:n_version)
	 XB[i] = beta[r, nonzero_X_idx[version_idx[r]]]
	
    }
}

model {
    //priors

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma ~ gamma(1.39435729464721, 0.39435729464721);

    /* for (v in 1:V_raw) */
    /*     theta_raw[v] ~ normal(prior_mean[v], prior_sd[v]); */
  theta ~ normal(prior_mean, prior_sd);

  L_omega ~ lkj_corr_cholesky(4);

    /* for (r in 1:R) */
    /*     standard_normal[r] ~ normal(0, 1); */
  standard_normal ~ normal(0, 1);
    //likelihood
  {
    int idx = 1;
    real[C] xb;
    for (r in 1:R) {
        for (s in 1:S) {
	  xb = segment(XB[version_idx[r]],idx, idx+C);
	  Y[r, s] ~ categorical_logit(xb);
	  idx = idx+C;
        }
    }
}
