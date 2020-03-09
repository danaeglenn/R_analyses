setwd("/Volumes/UCR-KINDLab/danag/OngoingAnalyses/UCLA_Aggression/SEM_analyses")

#install.packages("lavaan", dependencies=TRUE)

library(lavaan)
dat <- read.csv("SEM_data_full.csv", header=T)

colnames(dat) = c('sid','w1_proact','w1_react','AGG1','w2_proact','w2_react','AGG2','w3_proact','w3_react','AGG3','PEER1','PEER2','PEER3','ADHD1','ADHD2','ADHD3')

model.ADHDaggression1 <-
  '
#variances
ADHD1 ~~ ADHD1
AGG1 ~~ AGG1
PEER1 ~~ PEER1

ADHD2 ~~ ADHD2
AGG2 ~~ AGG2
PEER2 ~~ PEER2

ADHD3 ~~ ADHD3
AGG3 ~~ AGG3
PEER3 ~~ PEER3

#regressions
ADHD2 ~     ADHD1 + 0*AGG1 + 0*PEER1
AGG2  ~   a*ADHD1 +   AGG1 + 0*PEER1
PEER2 ~   0*ADHD1 + AGG1 +   PEER1

ADHD3 ~     ADHD2 + 0*AGG2 + 0*PEER2 + 0*ADHD1 + 0*AGG1 + 0*PEER1
AGG3  ~   a*ADHD2 +   AGG2 + 0*PEER2 + 0*ADHD1 + 0*AGG1 + 0*PEER1
PEER3 ~   0*ADHD2 + AGG2 +   PEER2 +  0*ADHD1 + 0*AGG1 + 0*PEER1

#covariances
#W1
ADHD1 ~~ AGG1
ADHD1 ~~ PEER1
AGG1 ~~ PEER1

#W2
ADHD2 ~~ AGG2
ADHD2 ~~ PEER2
#AGG2 ~~ PEER2

#W3
ADHD3 ~~ AGG3
ADHD3 ~~ PEER3
AGG3 ~~ PEER3

#sets covariance between timepoints to 0
ADHD1 ~~ 0*ADHD2
ADHD1 ~~ 0*AGG2
ADHD1 ~~ 0*PEER2
ADHD1 ~~ 0*ADHD3
ADHD1 ~~ 0*AGG3
ADHD1 ~~ 0*PEER3

AGG1 ~~ 0*ADHD2
AGG1 ~~ 0*AGG2
AGG1 ~~ 0*PEER2
AGG1 ~~ 0*ADHD3
AGG1 ~~ 0*AGG3
AGG1 ~~ 0*PEER3

PEER1 ~~ 0*ADHD2
PEER1 ~~ 0*AGG2
PEER1 ~~ 0*PEER2
PEER1 ~~ 0*ADHD3
PEER1 ~~ 0*AGG3
PEER1 ~~ 0*PEER3

ADHD2 ~~ 0*ADHD3
ADHD2 ~~ 0*AGG3
ADHD2 ~~ 0*PEER3

AGG2 ~~ 0*ADHD3
AGG2 ~~ 0*AGG3
AGG2 ~~ 0*PEER3

PEER2 ~~ 0*ADHD3
PEER2 ~~ 0*AGG3
PEER2 ~~ 0*PEER3
'

#use lavaan to run analysis
fit.path.agg <- sem(model=model.ADHDaggression1, data=dat, missing='fiml')
summary(fit.path.agg)
fitMeasures(fit.path.agg, fit.measures = "all")
inspect(fit.path.agg)