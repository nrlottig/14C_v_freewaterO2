data {
  // declare variables
  // indices
  int n_obs; // number of observations
  int n_years; // number of years
  int n_days; // number of days
  int n_series; // number of time series
  int map_days[n_obs]; // mapping of observations to days
  int days_per_year[n_years]; // number of days in each year
  int obs_per_series[n_series]; // number of obervsations in each time series
  int obs_per_day[n_days]; // number of observations in each day
  
  // actual data
  real<lower=0> o2_obs[n_obs]; // observed oxygen [mg m^-3]
  real<lower=0> o2_eq[n_obs]; // equilibrium oxygen [mg m^-3] 
  real<lower=0> light[n_obs]; // light [umol-photons m^-2 s^-1]
  real<lower=0> temp[n_obs]; // temperature [C]
  real<lower=0> wspeed[n_obs]; // wind speeed [m s^-1]
  real<lower=0> z[n_obs]; // mixing depth [m]
  real<lower=0> temp_ref; // reference temperature [C]
  real<lower=0> k[n_obs]; // k value for gas exchange [h^-1]
  real<lower=0> o2_freq;
  real<lower=0> sig_b0;
  real<lower=0> sig_r;
  real<lower=0> sig_i0;
}

transformed data {
  // declare variables
  real<lower=0> mu; // mean of DO for entire time series
  real<lower=0> tau; // sd of DO for entire time series
  real<lower=0> max_ik; //upper limit of ik
  real<lower=0> min_ik; //lower limit of ik
  real x_obs[n_obs]; // scaled observed DO
  real x_eq[n_obs]; // scaled equilibrium DO
  real nu[n_obs]; // scaled gas exchange
  real<lower=0> eta; // mean of light for entire time series
  real<lower=0> lambda[n_obs]; // scaled light 
  
  // scale data
  mu = mean(o2_obs);
  tau = sd(o2_obs);
  eta = mean(light);
  max_ik = 700/eta; //set upper limit of ik to 700 based on 14C incubation data
  min_ik = 20/eta; // set lower limit of ik to 50 based on 14C incubation data
  for (n in 1:n_obs){
    x_obs[n] = (o2_obs[n] - mu)/tau;
    x_eq[n] = (o2_eq[n] - mu)/tau;
    lambda[n] = light[n]/eta;
    nu[n] = k[n]*(x_eq[n] - x_obs[n]);
  }
}


parameters {
  real<lower=1> gamma_1; // scaling of gpp with temperature
  real<lower=1> gamma_2; // scaling of er with temperature
  real<lower=0> sig_proc; // sd of oxygen state process error
  real<lower=0> b0[n_days]; // scaled max gpp with photoinhibition
  real<lower=min_ik,upper=max_ik> i0[n_days]; // scaled light saturation value
  real<lower=0> r[n_days]; // scaled er at temp_ref
}


transformed parameters {
  // declare variables
  real b[n_obs]; // scaled max gpp at high light
  real chi[n_obs]; // scaled gpp 
  real kappa[n_obs]; // scaled er 
  real phi[n_obs]; // nep [g m^-2 h^-1]
  real x_pred[n_obs]; // predicted oxygen [g m^-3]

  // predicted oxygen 
  for (n in 1:n_obs) {
    b[n] = b0[map_days[n]]*gamma_1^(temp[n] - temp_ref);
    chi[n] = b[n]*((lambda[n]/i0[map_days[n]])*exp(1-lambda[n]/i0[map_days[n]]));
    kappa[n] = r[map_days[n]]*gamma_2^(temp[n] - temp_ref);
    phi[n] = chi[n] - kappa[n];
    x_pred[n] = x_obs[n] + phi[n] + nu[n];
  }
}


model {
 // priors
  gamma_1 ~ normal(1, 5); 
  gamma_2 ~ normal(1, 5);
  sig_proc ~ gamma(1.5, 1.5/0.1); 
  // daily parameters
    {
    int pos = 1;
    for (y in 1:n_years) {
      // inital value
      b0[pos] ~ exponential(1);
      r[pos] ~ exponential(1);
      i0[pos] ~ exponential(1);
      // random walk
      for (d in (pos+1):(pos+days_per_year[y]-1)) {
        b0[d] ~ normal(b0[d-1], sig_b0) T[0, ];
        r[d] ~ normal(r[d-1], sig_r) T[0, ]; 
        i0[d] ~ normal(i0[d-1], sig_i0) T[0, ]; 
      }
      pos = pos + days_per_year[y];
    }
  }
  // likelihood
  {
    int pos = 1; 
    for (t in 1:n_series) {
      x_obs[(pos+1):(pos+obs_per_series[t]-1)] 
        ~ normal(x_pred[pos:(pos+obs_per_series[t]-2)], sig_proc);
      pos = pos + obs_per_series[t];
    }
  }
}


generated quantities {
  real gpp[n_obs]; // gpp [mg m^-2 h^-1]
  real er[n_obs]; // er [mg m^-2 h^-1]
  real nep[n_obs]; // nep [mg m^-2 h^-1]
  real gpp_m2[n_obs]; // gpp [mg m^-2 h^-1]
  real er_m2[n_obs]; // er [mg m^-2 h^-1]
  real nep_m2[n_obs]; // nep [mg m^-2 h^-1]
  real GPP[n_days]; // total daily flux [g m^-2 d^-1]
  real ER[n_days]; // total daily flux [g m^-2 d^-1]
  real NEP[n_days]; // total daily flux [g m^-2 d^-1]
  real GPP_m2[n_days]; // total daily flux [g m^-2 d^-1]
  real ER_m2[n_days]; // total daily flux [g m^-2 d^-1]
  real NEP_m2[n_days]; // total daily flux [g m^-2 d^-1]
  real GPP_mean; // overall mean flux [g m^-2 d^-1]
  real ER_mean; // overall mean flux [g m^-2 d^-1]
  real NEP_mean; // overall mean flux [g m^-2 d^-1]
  real GPP_mean_m2; // overall mean flux [g m^-2 d^-1]
  real ER_mean_m2; // overall mean flux [g m^-2 d^-1]
  real NEP_mean_m2; // overall mean flux [g m^-2 d^-1]

  // back-tranformed scaled variables [g m^-2 d^-1]
  for(n in 1:n_obs){
    gpp[n] = tau*chi[n]/31.998;
    er[n] = tau*kappa[n]/31.998;
    nep[n] = tau*phi[n]/31.998;
    gpp_m2[n] = tau*chi[n]*z[n]/31.998;
    er_m2[n] = tau*kappa[n]*z[n]/31.998;
    nep_m2[n] = tau*phi[n]*z[n]/31.998;
  }
  {
    int pos = 1;
    for (d in 1:n_days){
      GPP[d] = o2_freq*mean(gpp[pos:(pos + obs_per_day[d] - 1)]);
      ER[d] = o2_freq*mean(er[pos:(pos + obs_per_day[d] - 1)]);
      NEP[d] = o2_freq*mean(nep[pos:(pos + obs_per_day[d] - 1)]);
      GPP_m2[d] = o2_freq*mean(gpp_m2[pos:(pos + obs_per_day[d] - 1)]);
      ER_m2[d] = o2_freq*mean(er_m2[pos:(pos + obs_per_day[d] - 1)]);
      NEP_m2[d] = o2_freq*mean(nep_m2[pos:(pos + obs_per_day[d] - 1)]);

      pos = pos + obs_per_day[d]; // advance position counter
  }
  // mean daily fluxes
    GPP_mean = mean(GPP);
    ER_mean = mean(ER);
    NEP_mean = mean(NEP);  
    GPP_mean_m2 = mean(GPP_m2);
    ER_mean_m2 = mean(ER_m2);
    NEP_mean_m2 = mean(NEP_m2);  
  }
}

