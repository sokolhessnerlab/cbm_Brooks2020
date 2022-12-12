data {
  int N; // number of trials total (across participants)
  int nsubj; // # of subjects
  int choices[N]; // the binary choice vector
  real gam1[N]; // the risky gain element
  real gam2[N]; // the risky loss element
  real cert[N]; // the guaranteed/certain alternative
  real potc[N];
  int ind[N]; // the subject index
}

parameters {
  real meanLambda; // Mean parameters
  real<lower=0> sdLambda;
  real meanRho;
  real<lower=0> sdRho;
  real meanMu;
  real<lower=0> sdMu;
  real l[nsubj]; // L = loss aversion
  real r[nsubj]; // R = curvature
  real m[nsubj]; // M = softmax
  real meanDB; // RFX decisionbias
  real<lower=0> sdDB;
  real db[nsubj];
  real meanadjDB; // Decision bias adjustment
  real<lower=0> sdadjDB;
  real meanadjL; // Lambda adjustment
  real<lower=0> sdadjL;
  real meanadjR; // Rho adjustment
  real<lower=0> sdadjR;
  real meanadjM; // Mu adjustment
  real<lower=0> sdadjM;
  real adjDB[nsubj];
  real adjL[nsubj];
  real adjR[nsubj];
  real adjM[nsubj];
}

transformed parameters {
  real ltmp[N];
  real rtmp[N];
  real mtmp[N];
  real dbtmp[N];
  real adjDBlim[nsubj];
  real adjLlim[nsubj];
  real adjRlim[nsubj];
  real adjMlim[nsubj];
  
  for(s in 1:nsubj){
    adjDBlim[s] = 1/(1+exp(-adjDB[s]))*2-1;
    adjLlim[s]  = 1/(1+exp(-adjL[s]))*2-1;
    adjRlim[s]  = 1/(1+exp(-adjR[s]))*2-1;
    adjMlim[s]  = 1/(1+exp(-adjM[s]))*2-1;
  }
  
  dbtmp[1] = db[ind[1]];
  ltmp[1] = exp(l[ind[1]]);
  rtmp[1] = exp(r[ind[1]]);
  mtmp[1] = exp(m[ind[1]]);
  for(t in 2:N){
    if(ind[t]!=ind[t-1]){
      dbtmp[t]   = db[ind[t]];
      ltmp[t] = exp(l[ind[t]]);
      rtmp[t] = exp(r[ind[t]]);
      mtmp[t] = exp(m[ind[t]]);
    } else {
      dbtmp[t]    = dbtmp[t-1]     + potc[t]*adjDBlim[ind[t]];
      ltmp[t] = exp(log(ltmp[t-1]) + potc[t]*adjLlim[ind[t]]);
      rtmp[t] = exp(log(rtmp[t-1]) + .25*potc[t]*adjRlim[ind[t]]);
      mtmp[t] = exp(log(mtmp[t-1]) + .25*potc[t]*adjMlim[ind[t]]);
    }
  }
  
}

model {
  real div;
  real p[N];
  real gam1u;
  real gam2u;
  real certu;
  
  // Priors (don't matter a TON)
   meanLambda ~ normal(0,30);
   sdLambda   ~ cauchy(0,2.5);
   meanMu ~ uniform(0,30);
   sdMu   ~ cauchy(0,2.5);
   meanRho ~ normal(0,30);
   sdRho   ~ cauchy(0,2.5);
   meanDB ~ normal(0,30);
   sdDB ~ cauchy(0,2.5);
   meanadjDB ~ normal(0,10);
   sdadjDB ~ cauchy(0,2.5);
   meanadjL ~ normal(0,10);
   sdadjL ~ cauchy(0,2.5);
   meanadjR ~ normal(0,10);
   sdadjR ~ cauchy(0,2.5);
   meanadjM ~ normal(0,10);
   sdadjM ~ cauchy(0,2.5);

   // Hierarchy
   l ~ normal(meanLambda,sdLambda);
   m ~ normal(meanMu,sdMu);
   r ~ normal(meanRho,sdRho);
   db ~ normal(meanDB,sdDB);
   adjDB ~ normal(meanadjDB,sdadjDB);
   adjL ~  normal(meanadjL,sdadjL);
   adjR ~  normal(meanadjR,sdadjR);
   adjM ~  normal(meanadjM,sdadjM);
   
   for (t in 1:N) {
   div = 30^rtmp[t];
   
   // Model with M, L, R
   if (gam1[t] < 0)
   gam1u = -0.5 * ltmp[t] * fabs(gam1[t])^rtmp[t];
   else
   gam1u = 0.5 * gam1[t]^rtmp[t];
   
   if (gam2[t] < 0)
   gam2u = -0.5 * ltmp[t] * fabs(gam2[t])^rtmp[t];
   else
   gam2u = 0.5 * gam2[t]^rtmp[t];
   
   if (cert[t] < 0)
   certu = -ltmp[t] * fabs(cert[t])^rtmp[t];
   else
   certu = cert[t]^rtmp[t];
   
   p[t] = inv_logit(mtmp[t] / div * (gam1u + gam2u - certu - dbtmp[t]));
   }
   choices ~ bernoulli(p);  
}
