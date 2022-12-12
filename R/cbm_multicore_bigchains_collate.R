# Choice behavior expected subjective value theory
# created by hr: 05/01/17
# this script uploads and cleans data, creates function for ESVT + data analysis
#-----------------------------------------------------------------------
#                     Import & Clean Data
#-----------------------------------------------------------------------
# 1) #clear the environment. ctrl + l clears console
rm(list=ls());
# library(tictoc);
# library(lme4);

# 2) Load data. Name it "dirty", header = FALSE bc 1st row in csv file is not the list of variables
library(readr); #find package readr to read csv file
dirty <- read.csv("~/Documents/Dropbox/Libraries/R_lib/CBM/collatedchoicedata_LAP_IDM_PDM_CDM.csv",header = FALSE, col.names = c("riskyGain", "riskyLoss","alternative", "choice","outcome","rxTime","day","trial",
                                                                                                                                 "studyNum","regulation","propranolol","stress","withinSubsIndex","subjectIndex")); #using ~before path file puts you in the home folder
genAge<- read.csv("~/Documents/Dropbox/Libraries/R_lib/CBM/allgendersages.csv", header=TRUE);
#dirty <- read.csv("/Users/hayley/Desktop/shlab data/rdata/collatedchoicedata_LAP_IDM_PDM_CDM.csv",header = FALSE, col.names = c("riskyGain", "riskyLoss","alternative", "choice","outcome","rxTime","day","trial",
#"studyNum","regulation","propranolol","stress","withinSubsIndex","subjectIndex"));
#genAge<- read.csv("/Users/hayley/Desktop/shlab data/rdata/allgendersages.csv", header = TRUE);

#in genAge, there are 128 females and 106 males, 234 total subjects.

# 3)adding age and gender to entire dataset
gamtx = matrix(nrow=64953, ncol = 5, dimnames =list(c(NULL), c("studyN", "studysubjInd","subjInd", "gender","age"))); #gender and age
gamtx= as.data.frame(gamtx); #make gamtx into data frame
old <-as.vector(unique(dirty$subjectIndex)); #create vector with each subject index from dirty, 1:234
#this for loop takes each row from genAge and assigns it to the corresponding subject from dirty and puts it into gamtx
for (i in 1:length(old)){
  gamtx$studyN[dirty$subjectIndex==old[i]] <- genAge$studyN[i]
  gamtx$studysubjInd[dirty$subjectIndex==old[i]] <- genAge$studysubjInd[i]
  gamtx$subjInd[dirty$subjectIndex==old[i]] <- genAge$subjInd[i]
  gamtx$gender[dirty$subjectIndex==old[i]] <- genAge$gender[i]
  gamtx$age[dirty$subjectIndex==old[i]] <- genAge$age[i]
};

#we can compare studyNum, withinSubsIndex, and subject Index with that of gamtx. It should be the same so we
#don't need to include those columns from gamtx when we combine with dirty, it is redundant.
plot(dirty$studyNum, gamtx$studyN);
plot(dirty$withinSubsIndex, gamtx$studysubjInd);
plot(dirty$subjectIndex, gamtx$subjInd);
dirty[,15:16] = gamtx[,4:5]; #they are the same, so only need to add gender and age to dirty

# 4) Cleaning the data: Only subjects w/ no propranolol, regulation, stress, and from day 1 of study
cleanData <- dirty[(dirty$propranolol=="0")&(dirty$regulation=="0")&(dirty$stress=="0")&(dirty$day=="1"),];

# 5) check cleanData 
plot(cleanData$propranolol); #should be @ zero
plot(cleanData$regulation); #should be @ zero
plot(cleanData$stress); #should be @ zero
plot(cleanData$day); #should be @ one

# 6) how many people and choices are there in clean dataset? 
nrow(cleanData); #how many rows( (total trials) in cleanData are there?
length(unique(cleanData$subjectIndex)); #how many participants?

# 7) Reassigning subject index
#When we subset data, we removed some subjects 
unique(diff(cleanData$subjectIndex)); #it will be inconsistent for now, we will make it 0 1
max(unique(cleanData$subjectIndex)); #what is the largest unique number in subjectIndex of cleanData?
oldNum<-as.vector(unique(cleanData$subjectIndex));#create a new vector called oldNum with the number of unique 
#numbers from subjectIndex of cleanData dataframe which is 
#1:151 after subsetting the data
newNum<-as.vector(1:nrow(cleanData)); #create new vector called newNum with 23373 rows, this corresponds with the number 
#of subjects we have in the dataframe after subsetting the 'dirty' frame

for (i in 1:length(oldNum)) 
{newNum[cleanData$subjectIndex==oldNum[i]] <- i}; 
#go through cleanData$subjectIndex and when the number is on the ith unique number, replace with ith value equivalent to loop we are on.assign to newNum vector.
plot(newNum,cleanData$subjectIndex); #plot newNum by cleanData$subjectIndex
length(unique(cleanData$subjectIndex)); #what is the length of unique numbers in cleanData$subjectIndex
max(unique(newNum)); #this should now be the same as above 
length(unique(newNum)); #what is the length of the unique numbers in newNum?
unique(diff(newNum)); #should give 0 1, meaning that there is only 1 step difference bewteen unique subject numbers
cleanData["NewSubjectIndex"]<-newNum; #add new num to cleanData set as cleanData$NewSubjectIndex. Old index still there
plot(cleanData$NewSubjectIndex)

#cleanData gender input is integer, 1 for females, 2 for males so we subtract 1, so 0=females, 1=males
cleanData$gender <-cleanData$gender-1 #0 for females, 1 for males

# 8) Create shorter matrix with only subject info included in cleanData
cleanSubind <- unique(cleanData$NewSubjectIndex); #create index with number of subejcts
cleanSub <- matrix(nrow = length(cleanSubind), ncol = 7, dimnames = list(c(NULL),c("trial","studyNum","withinSubsIndex","subjectIndex","gender","age","NewSubjectIndex")));
cleanSub <- cleanData[which(diff(cleanData$NewSubjectIndex)==1),c(-1:-7,-10:-12)]; #for rows when diff is 1, put columns 8,9,13-17 in cleanSub 
cleanSub[151,] <- cleanData[nrow(cleanData),c(-1:-7,-10:-12)]; #diff leaves out the last subject, add in
hist(cleanSub$age); #positive skewed
hist(log(cleanSub$age-14)); #log for clean subjectindex
sum(cleanSub$gender== 0); #88 females
sum(cleanSub$gender == 1); #63 males
row.names(cleanSub) <- 1:nrow(cleanSub)

# 8) Expected Values= Gain*Prob + Loss*Prob + Alt*Prob
cleanData$meanEV<-((cleanData$riskyGain)*.5 + (cleanData$riskyLoss)*.5 + cleanData$alternative)/2; 
# cleanData$pastEV<-((cleanData$riskyGain)*.5) + ((cleanData$riskyLoss)*.5) + ((cleanData$alternative)*1); #these will get shifted down one for past trial comparison
nT = length(cleanData$NewSubjectIndex); # numbers of trials
firstT = which(diff(cleanData$NewSubjectIndex)==1)+1; # an index telling me which is the first trial for each subject.

pastevent <-function(v,firstT){
  nT = length(v);
  newv = c(NaN,v[1:(nT-1)]);
  newv[firstT] = NaN;
  return(newv);
}

cleanData$pastmeanEV = pastevent(cleanData$meanEV,firstT);
cleanData$pastriskyGain = pastevent(cleanData$riskyGain,firstT);
cleanData$pastriskyLoss = pastevent(cleanData$riskyLoss,firstT);
cleanData$pastalternative = pastevent(cleanData$alternative,firstT);

cleanData$win = as.numeric(cleanData$outcome == cleanData$riskyGain); 
cleanData$loss = as.numeric(cleanData$outcome == cleanData$riskyLoss); 
cleanData$alt = as.numeric(cleanData$outcome == cleanData$alternative); 

cleanData$pastWin = pastevent(cleanData$win,firstT);
cleanData$pastLoss = pastevent(cleanData$loss,firstT);
cleanData$pastAlt = pastevent(cleanData$alt,firstT);

cleanData$pastWinAmt = pastevent(cleanData$win*cleanData$riskyGain,firstT)
cleanData$pastLossAmt = pastevent(cleanData$loss*cleanData$riskyLoss,firstT)
cleanData$pastAltAmt = pastevent(cleanData$alt*cleanData$alternative,firstT)

cleanData$pastChoice = pastevent(cleanData$choice,firstT);
cleanData$pastOutcome = pastevent(cleanData$outcome,firstT);

cleanData$ppChoice = pastevent(cleanData$pastChoice,firstT);
cleanData$pppChoice = pastevent(cleanData$ppChoice,firstT);
cleanData$ppOutcome = pastevent(cleanData$pastOutcome,firstT);
cleanData$pppOutcome = pastevent(cleanData$ppOutcome,firstT);
cleanData$ppEV = pastevent(cleanData$pastmeanEV,firstT);
cleanData$pppEV = pastevent(cleanData$ppEV,firstT);

cleanData$poutcomeVal = cleanData$pastWin - cleanData$pastLoss + cleanData$pastAlt;
cleanData$poutcomeVal2 = (cleanData$pastOutcome > 0) - (cleanData$pastOutcome < 0);
cleanData$pwinloss = cleanData$pastWin - cleanData$pastLoss

tmpcumearnings = vector(length=nT);
trials = vector(length=nT);
trials[1] = 1;
tmpcumearnings[1] = 0;
for (t in 2:nT){
  if (cleanData$NewSubjectIndex[t] == cleanData$NewSubjectIndex[t-1]){
    tmpcumearnings[t] = cleanData$outcome[t-1] + tmpcumearnings[t-1];
    trials[t] = trials[t-1] + 1;
  }
  else if (cleanData$NewSubjectIndex[t] != cleanData$NewSubjectIndex[t-1]){
    tmpcumearnings[t] = 0;
    trials[t] = 1;
  }
}
cleanData$cumearnings = tmpcumearnings;
cleanData$trials = trials;
# THIS IS JUST THE NUMBER TRIAL IN CLEANDATA FOR THAT SUBJECT. 
# THE $TRIAL IS THE NUMBER TRIAL OVERALL FOR THAT SUBJECT (SEE: ATTEND/REGULATE SUBJECTS)

cleanData$cumearningsReS = cleanData$cumearnings/max(cleanData$cumearnings);
cleanData$trialsReS = cleanData$trials/max(cleanData$trials);
cleanData$pastOutcomeReS = cleanData$pastOutcome/max(cleanData$pastOutcome,na.rm=TRUE);

for (s in 1:length(cleanSubind)){
  tmpdata = cleanData[cleanData$NewSubjectIndex==s,];
  
  tmpvol = vector(length=length(tmpdata[,1]));
  tmpvol[1:2] = 0;
  
  for (t in 3:length(tmpvol)){
    ll = max(c(t-25,1));
    ul = t-1;
    tmpvol[t] = var(tmpdata$outcome[ll:ul])
  }
  
  cleanData$ocvol25[cleanData$NewSubjectIndex==s] = tmpvol/mean(tmpvol);
}

### DO BAYESIAN ESTIMATION ###

library(rstan)
library(parallel)
# set_cppo('debug') # make debugging easier
# set_cppo('fast') # for best running speed
setwd('/Users/sokolhessner/Documents/Dropbox/Libraries/R_lib/CBM/')

cleanData$pastOutcomeReSstan = cleanData$pastOutcomeReS;
cleanData$pastOutcomeReSstan[is.na(cleanData$pastOutcomeReS)] = 0;

dataList <- list(
  choices = cleanData$choice ,
  gam1 = cleanData$riskyGain ,
  gam2 = cleanData$riskyLoss ,
  cert = cleanData$alternative ,
  potc = cleanData$pastOutcomeReSstan,
  ocvol25 = cleanData$ocvol25,
  ind = cleanData$NewSubjectIndex ,
  nsubj = length(unique(cleanData$NewSubjectIndex)) , 
  N = length(cleanData$choice)
)


# stanmodel = 'cbm_basic_01.stan'
# stanmodel = 'cbm_basic_01_constantR.stan' # for a constant shared classic reference point
# stanmodel = 'cbm_basic_01_RFXconstantR.stan' # for a constant RFX classic reference point
# stanmodel = 'cbm_basic_02_FFXdecisionbias.stan' # for a constant FFX decision bias
stanmodel = 'cbm_basic_02_RFXdecisionbias.stan' # for a constant RFX decision bias
# stanmodel = 'cbm_basic_02_RFXdecisionbias_FFXadjpotc.stan' # for an FFX-adjusting RFX decision bias
# stanmodel = 'cbm_basic_02_RFXdecisionbias_RFXadjpotc.stan' # for an RFX-adjusting RFX decision bias
# stanmodel = 'cbm_basic_02_RFXall_RFXadjpotcall.stan' # for RFX-adjusting RFX all parameters
# stanmodel = 'cbm_basic_02_RFXall_RFXadjpotcall_RFXadjocvolall.stan' # RFX approach to prev. otc and otc volatility
# stanmodel = 'cbm_basic_02_RFXall_RFXadjocvolall.stan' # RFX approach to otc volatility ONLY (no effect of prev. otcs)
# stanmodel = 'cbm_basic_02_RFXall_FFXadjpotcall_FFXadjocvolall.stan' # FFX approach to prev. otc and otc volatility

# stanmodel = 'cbm_basic_02_RFXdecisionbias_RFXadjpotc_TRANSISTIONAL.stan'

nChains = 8
fitSteps = 10000 # Stan will save half this many x nChains per parameter
pars = c('meanLambda','meanRho','meanMu',
         'sdLambda','sdRho','sdMu',
         # 'meanrp', 'sdrp','rp',
         'meanDB','sdDB','db',
         'meanadjDB','sdadjDB','adjDB',
         'meanadjL','sdadjL','adjL',
         'meanadjR','sdadjR','adjR',
         'meanadjM','sdadjM','adjM',
         # 'ocvolmeanadjDB','ocvolsdadjDB','ocvoladjDB',
         # 'ocvolmeanadjL','ocvolsdadjL','ocvoladjL',
         # 'ocvolmeanadjR','ocvolsdadjR','ocvoladjR',
         # 'ocvolmeanadjM','ocvolsdadjM','ocvoladjM',
         'l','r','m'); # Not including all the rtmp, ltmp, and mtmp "parameters"

# pars = c('meanLambda','meanRho','meanMu',
#          'sdLambda','sdRho','sdMu',
#          # 'meanrp', 'sdrp','rp',
#          'meanDB','sdDB','db',
#          'meanadjDB',
#          'meanadjL',
#          'meanadjR',
#          'meanadjM',
#          'ocvolmeanadjDB',
#          'ocvolmeanadjL',
#          'ocvolmeanadjR',
#          'ocvolmeanadjM',
#          'l','r','m'); # Not including all the rtmp, ltmp, and mtmp "parameters"


# ##### Fit the Model #####
# 
# starttime = proc.time()[3];
# 
# # seed <- 123; # for a reproducible fit
# seed <- runif(1,1,1e6); # Stan wants a random integer from 1 to max supportable
# 
# fit0 <- stan(file = stanmodel, data = dataList, 
#              iter = 1, chains = 1, pars=pars) #, init=init1)
# 
# fit0time = proc.time()[3];
# print(noquote(sprintf('Compilation time = %.1f seconds',(fit0time-starttime))))
# 
# ##### Parallel Model Fit #####
# 
# # seed <- 123; # for a reproducible fit
# seed <- runif(1,1,1e6); # Stan wants a random integer from 1 to max supportable
# 
# sflist1 <- 
#   mclapply(1:nChains, mc.cores = nChains, 
#            function(i) stan(fit = fit0, seed=seed, data = dataList,
#                             iter = fitSteps, chains = 1, chain_id = i,
#                             pars = pars))
# 
# fittime = proc.time()[3];
# print(noquote(sprintf('Sampling time = %.1f minutes.',(fittime-fit0time)/60)))

sflistFinal = list();
k = 1;
for (i in 1:nChains){
  if (any(dim(sflist1[[i]]) > 0)) {
    sflistFinal[[k]] <- sflist1[[i]]
    k = k + 1;
  }
  else {print(noquote(sprintf('WARNING: Chain %d did not include any samples.',i)))}
}

# save(stanmodel,sflistFinal,
#      file=sprintf('cbm_basic_01_%s.Rdata',format(Sys.Date(), format="%Y%m%d")))
# save(stanmodel,sflistFinal,
#      file=sprintf('cbm_basic_02_RFXall_RFXadjocvolall_%s.Rdata',format(Sys.Date(), format="%Y%m%d")))
# save(stanmodel,sflistFinal,
#      file=sprintf('cbm_basic_02_RFXall_RFXadjpotcall_node38_%s.Rdata',format(Sys.Date(), format="%Y%m%d")))

# fit1 <- sflist2stanfit(sflistFinal)
# 
# sSamples <- extract(fit1)
# q95 <- c(0.025,0.975);q90 <- c(0.05,0.95);q85 <- c(0.075,0.925);q80 <- c(0.1,0.9);q75 <- c(0.125,0.875);q70 <- c(0.15,0.85)
# 
# print(fit1)


# BEFORE THIS, CHECK THE CHAINS USING TRACEPLOT TO MAKE SURE THERE ARE NO OBVIOUSLY BAD CHAINS

# # WITH ADJUSTMENT TERMS
# load('cbm_basic_02_RFXall_RFXadjpotcall_node37_20200214.Rdata')
# sflist1 = sflistFinal;
# load('cbm_basic_02_RFXall_RFXadjpotcall_node38_20200214.Rdata')
# sflist2 = sflistFinal;
# load('cbm_basic_02_RFXall_RFXadjpotcall_node38_20200215.Rdata')
# sflist3 = sflistFinal;

# WITHOUT ADJUSTMENT TERMS
load('cbm_basic_02_RFXall_nopotc_hm1_20200227.Rdata')
sflist1 = sflistFinal;
load('cbm_basic_02_RFXall_nopotc_node41_20200227.Rdata') # edited to remove chains 5, 8, and 9; 5 had a bad stretch, 8 & 9 also removed so final # chains = 20
sflist2 = sflistFinal;
load('cbm_basic_02_RFXall_nopotc_node42_20200227.Rdata')
sflist3 = sflistFinal;

lengthlist1 = length(sflist1);
lengthlist2 = length(sflist2);
lengthlist3 = length(sflist3);
sflistFinal = list();

for (i in 1:lengthlist1){
  sflistFinal[[i]] <- sflist1[[i]];
}

for (j in 1:lengthlist2){
  sflistFinal[[lengthlist1 + j]] <- sflist2[[j]];
}

for (k in 1:lengthlist3){
  sflistFinal[[lengthlist1 + lengthlist2 + k]] <- sflist3[[k]];
}

save(stanmodel,sflistFinal,
     file=sprintf('cbm_basic_02_RFXall_nopotc_hpcCollated_%s.Rdata',format(Sys.Date(), format="%Y%m%d")))

fit1 <- sflist2stanfit(sflistFinal)

sSamples <- extract(fit1)
q95 <- c(0.025,0.975);q90 <- c(0.05,0.95);q85 <- c(0.075,0.925);q80 <- c(0.1,0.9);q75 <- c(0.125,0.875);q70 <- c(0.15,0.85)

print(fit1)

fit1summ = summary(fit1);

neffs = fit1summ$summary[,'n_eff'];
rhats = fit1summ$summary[,'Rhat'];


meanparams = c('meanLambda','meanRho','meanMu','meanDB',
              'meanadjDB','meanadjL','meanadjR','meanadjM')

sdparams = c('sdLambda','sdRho','sdMu','sdDB',
             'sdadjDB','sdadjL','sdadjR','sdadjM')

neffs[meanparams]
rhats[meanparams]

neffs[sdparams]
rhats[sdparams]

# Means and 95% CIs for main parameters
exp(mean(sSamples$meanRho))
exp(quantile(probs=q95,sSamples$meanRho))

exp(mean(sSamples$meanLambda))
exp(quantile(probs=q95,sSamples$meanLambda))

exp(mean(sSamples$meanMu))
exp(quantile(probs=q95,sSamples$meanMu))

mean(sSamples$meanDB)
quantile(probs=q95,sSamples$meanDB)

# Means and 95% CIs for adjustment parameters
1/(1+exp(-mean(sSamples$meanadjL)))*2-1
1/(1+exp(-quantile(probs=q95,sSamples$meanadjL)))*2-1

0.25*(1/(1+exp(-mean(sSamples$meanadjM)))*2-1)
0.25*(1/(1+exp(-quantile(probs=q95,sSamples$meanadjM)))*2-1)

1/(1+exp(-mean(sSamples$meanadjDB)))*2-1
1/(1+exp(-quantile(probs=q95,sSamples$meanadjDB)))*2-1

0.25*(1/(1+exp(-mean(sSamples$meanadjR)))*2-1)
0.25*(1/(1+exp(-quantile(probs=q95,sSamples$meanadjR)))*2-1)



# pars = c('meanLambda','meanRho','meanMu',
#          'sdLambda','sdRho','sdMu',
#          # 'meanrp', 'sdrp','rp',
#          'meanDB','sdDB','db',
#          'meanadjDB','sdadjDB','adjDB',
#          'meanadjL','sdadjL','adjL',
#          'meanadjR','sdadjR','adjR',
#          'meanadjM','sdadjM','adjM',
#          # 'ocvolmeanadjDB','ocvolsdadjDB','ocvoladjDB',
#          # 'ocvolmeanadjL','ocvolsdadjL','ocvoladjL',
#          # 'ocvolmeanadjR','ocvolsdadjR','ocvoladjR',
#          # 'ocvolmeanadjM','ocvolsdadjM','ocvoladjM',
#          'l','r','m'); # Not including all the rtmp, ltmp, and mtmp "parameters"


adjRm = 0.25*(1/(1+exp(-sSamples$meanadjR))*2-1)
adjLm = 1/(1+exp(-sSamples$meanadjL))*2-1
adjMm = 0.25*(1/(1+exp(-sSamples$meanadjM))*2-1)
adjDBm = 1/(1+exp(-sSamples$meanadjDB))*2-1

adjLs2 = mean(1/(1+exp(-sSamples$adjL[,37]))*2-1);
adjLs3 = mean(1/(1+exp(-sSamples$adjL[,118]))*2-1);
adjLs5 = mean(1/(1+exp(-sSamples$adjL[,134]))*2-1);

adjLs = colMeans(1/(1+exp(-sSamples$adjL))*2-1);


s2potc = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==37];
s3potc = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==118];
s5potc = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==134];
# mL = mean(sSamples$meanLambda);
s2L = c(); s3L = c(); s5L = c();
sL = array(data=NA,dim=c(ncol(sSamples$l),180));
for(s in 1:ncol(sSamples$l)){
  potc = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==s];
  sL[s,1] = exp(mean(sSamples$l[,s]));
  for(t in 2:length(potc)){
    sL[s,t] = exp(log(sL[s,t-1]) + potc[t]*adjLs[s]);
  }
}
plot(1:140,sL[1,1:140],type='l',ylim=c(0,7),xlim=c(1,180))
for(s in 2:ncol(sSamples$l)){
  nt = sum(is.finite(sL[s,]));
  ts = 1:nt;
  if(adjLs[s] < 0){
    lines(ts,sL[s,1:nt],col="gray")
  } else if(adjLs[s] < 0.02){
    lines(ts,sL[s,1:nt],col="blue")
  } else {
    lines(ts,sL[s,1:nt],col="red")
  }
}

for(s in 1:ncol(sSamples$l)){
  nt = sum(is.finite(sL[s,]));
  ts = 1:nt;
  pdf(sprintf('lambdaS%d.pdf',s));
  plot(ts,sL[s,1:nt],type='l')
  dev.off()
  # Sys.sleep(0.5)
}

s2L[1] = exp(mean(sSamples$l[,37]));
s3L[1] = exp(mean(sSamples$l[,118]));
s5L[1] = exp(mean(sSamples$l[,134]));
# s1adL = mean(adjLm);
for(t in 2:length(s2potc)){
  s2L[t] = exp(log(s2L[t-1]) + s2potc[t]*adjLs2);
}
for(t in 2:length(s3potc)){
  s3L[t] = exp(log(s3L[t-1]) + s3potc[t]*adjLs3);
}
for(t in 2:length(s5potc)){
  s5L[t] = exp(log(s5L[t-1]) + s5potc[t]*adjLs5);
}

ts2 = 1:length(s2L);
ts3 = 1:length(s3L);
ts5 = 1:length(s5L);
plot(ts2,s2L,type='l',ylim=c(1.6,3))
lines(ts3,s3L)
lines(ts5,s5L)


# Let's just do it with the mean and show the effect of different histories
mL = exp(mean(sSamples$meanLambda));
madjLm = mean(adjLm);
mM = exp(mean(sSamples$meanMu));
madjMm = mean(adjMm);
mDB = mean(sSamples$meanDB);
madjDBm = mean(adjDBm);

potc1 = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==5];
potc2 = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==10];
potc3 = cleanData$pastOutcomeReSstan[cleanData$NewSubjectIndex==16];

ltrajectory = array(data=NA,dim=c(3,140));
ltrajectory[,1] = mL;

mtrajectory = array(data=NA,dim=c(3,140));
mtrajectory[,1] = mM;

DBtrajectory = array(data=NA,dim=c(3,140));
DBtrajectory[,1] = mDB;

for(t in 2:140){
  ltrajectory[1,t] = exp(log(ltrajectory[1,t-1]) + potc1[t]*madjLm);
  ltrajectory[2,t] = exp(log(ltrajectory[2,t-1]) + potc2[t]*madjLm);
  ltrajectory[3,t] = exp(log(ltrajectory[3,t-1]) + potc3[t]*madjLm);
  
  mtrajectory[1,t] = exp(log(mtrajectory[1,t-1]) + potc1[t]*madjMm);
  mtrajectory[2,t] = exp(log(mtrajectory[2,t-1]) + potc2[t]*madjMm);
  mtrajectory[3,t] = exp(log(mtrajectory[3,t-1]) + potc3[t]*madjMm);
  
  DBtrajectory[1,t] = DBtrajectory[1,t-1] + potc1[t]*madjDBm;
  DBtrajectory[2,t] = DBtrajectory[2,t-1] + potc2[t]*madjDBm;
  DBtrajectory[3,t] = DBtrajectory[3,t-1] + potc3[t]*madjDBm;
  
}

plot(1:140,ltrajectory[1,],type='l',col='red',ylim=c(1.55,1.7))
lines(1:140,ltrajectory[2,],col='orange')
lines(1:140,ltrajectory[3,],col='dark red')

plot(1:140,mtrajectory[1,],type='l',col='pink')#,ylim=c(1.55,1.7))
lines(1:140,mtrajectory[2,],col='purple')
lines(1:140,mtrajectory[3,],col='deep pink')

plot(1:140,DBtrajectory[1,],type='l',col='blue')#,ylim=c(1.55,1.7))
lines(1:140,DBtrajectory[2,],col='cyan')
lines(1:140,DBtrajectory[3,],col='navy blue')