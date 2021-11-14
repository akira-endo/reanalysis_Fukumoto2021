data <- read.csv("../Fukumoto2021/covid.csv")

# Apr 6
source("../Fukumoto2021/preprocess/preprocess.0406.R") 
 pref.dummy <- as.data.frame(pref.dummy)
 outcome <- as.data.frame(outcome)
data_extend<-cbind(data.analysis,pref.dummy,treat)
wform<-as.formula(paste0("treat ~ 
                     prec_mean + shine_mean +
                     tmean_mean +
                     electoral.time +
                     age.0406 +
                     win_count.0406 +
                     log.number +
                     shutdown.0304 + shutdown.0316 +
                     prior.infection.per.capita+", paste(c(paste0("cases_",1:7), colnames(control), colnames(pref.dummy)),
                                                           collapse="+")))
W.out0406 <- weightit(wform,
                    data = data_extend, 
                   estimand = "ATC", method = "cbps",include.obj=T)
W.out0406$data=data_extend

# Apr 10
source("../Fukumoto2021/preprocess/preprocess.0410.R")
 pref.dummy <- as.data.frame(pref.dummy)
 outcome <- as.data.frame(outcome)
data_extend<-cbind(data.analysis,pref.dummy,treat)
wform<-as.formula(paste0("treat ~ 
                     prec_mean + shine_mean +
                     tmean_mean +
                     electoral.time +
                     age.0410 +
                     win_count.0410 +
                     log.number +
                     shutdown.0304 + shutdown.0316 + shutdown.0406 +
                     prior.infection.per.capita+", paste(c(paste0("cases_",1:7), colnames(control), colnames(pref.dummy)),
                                                         collapse="+")))
 W.out0410 <- weightit(wform,
                    data = data_extend,
                   estimand = "ATC", method = "cbps",include.obj=T)
W.out0410$data=data_extend