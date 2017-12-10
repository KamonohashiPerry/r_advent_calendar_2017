data {
  int N;
  real Hit_Points[N];
  real Attack[N];
  real Defense[N];
  real Special_Attack[N];
  real Special_Defense[N];
  real Speed[N];
  int<lower=0, upper=1> Y[N];
}


parameters {
  real b[7];
}


model {
  for (n in 1:N)
    Y[n] ~ bernoulli_logit(b[1] + b[2]*Hit_Points[n] + b[3]*Attack[n] + b[4]*Defense[n] + b[5]*Special_Attack[n] + b[6]*Special_Defense[n] + b[7]*Speed[n]);
}

