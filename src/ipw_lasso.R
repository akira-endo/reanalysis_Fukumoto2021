library(WeightIt)
library(survey)
library(tidyverse)
library(CBPS)
library(glmnet)
library(glmnetUtils)
data <- read.csv("../Fukumoto2021/covid.csv")

# Apr 6
source("../Fukumoto2021/preprocess/preprocess.0406.R") 
 pref.dummy <- as.data.frame(pref.dummy)
 outcome <- as.data.frame(outcome)
data_extend<-cbind(data.analysis,pref.dummy,treat)
lassoform<-as.formula(paste0("treat ~ 
                     prec_mean + shine_mean +
                     tmean_mean +
                     electoral.time +
                     age.0406 +
                     win_count.0406 +
                     log.number +
                     shutdown.0304 + shutdown.0316 +
                     prior.infection.per.capita+", paste(c(paste0("cases_",1:7), colnames(control), colnames(pref.dummy)),
                                                           collapse="+")))
cvl0406 <- cv.glmnet(lassoform,
                    data = data_extend, # this argument might be mandatory
                   family=binomial(link="logit"))
selectedvars<-rownames(coef(cvl0406,s="lambda.min"))[(coef(cvl0406,s="lambda.min")!=0)%>%as.matrix%>%c][-1]
weightitform<-as.formula(paste0("treat ~ ",paste(selectedvars,collapse="+")))
wlout <- weightit(weightitform,
                    data = data_extend, # this argument might be mandatory
                   estimand = "ATC", ps=plogis(predict(cvl0406,s="lambda.min",newdata=data_extend)%>%c))
cvl0406$data=data_extend
cvl0406$selectedvars=selectedvars
cvl0406$outcome=outcome
cvl0406$YMD=YMD
cvl0406$part=117:145 # indicates March 30 to April 27
cvl0406$ps<-wlout$ps
cvl0406$weights=wlout$weights
cvl0406$wout<-wlout

saveRDS(cvl0406,"../output/ipw_lasso_0406.rds")

# Apr 10
source("../Fukumoto2021/preprocess/preprocess.0410.R") 
 pref.dummy <- as.data.frame(pref.dummy)
 outcome <- as.data.frame(outcome)
data_extend<-cbind(data.analysis,pref.dummy,treat)
lassoform<-as.formula(paste0("treat ~ 
                     prec_mean + shine_mean +
                     tmean_mean +
                     electoral.time +
                     age.0410 +
                     win_count.0410 +
                     log.number +
                     shutdown.0304 + shutdown.0316 + shutdown.0406 +
                     prior.infection.per.capita+", paste(c(paste0("cases_",1:7), colnames(control), colnames(pref.dummy)),
                                                         collapse="+")))
cvl0410 <- cv.glmnet(lassoform,
                    data = data_extend, # this argument might be mandatory
                   family=binomial(link="logit"))
selectedvars<-rownames(coef(cvl0410,s="lambda.min"))[(coef(cvl0410,s="lambda.min")!=0)%>%as.matrix%>%c][-1]
weightitform<-as.formula(paste0("treat ~ ",paste(selectedvars,collapse="+")))
wlout <- weightit(weightitform,
                    data = data_extend, # this argument might be mandatory
                   estimand = "ATC", ps=plogis(predict(cvl0410,s="lambda.min",newdata=data_extend)%>%c))
#wlout$covs=data_extend
cvl0410$data=data_extend
cvl0410$selectedvars=selectedvars
cvl0410$outcome=outcome
cvl0410$YMD=YMD
cvl0410$part=121:149 # indicates April 3 to May 1
cvl0410$ps<-wlout$ps
cvl0410$weights=wlout$weights
cvl0410$wout<-wlout
                    
saveRDS(cvl0410,"../output/ipw_lasso_0410.rds")