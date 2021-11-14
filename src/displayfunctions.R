library(WeightIt)
library(survey)
library(tidyverse)
library(CBPS)
library(glmnet)
library(glmnetUtils)
balplot=function(wout){
    wo<-wout
    wo$ps[wo$treat==1][1]=wo$ps[wo$treat==1][1]+1e-7
    wo$ps[wo$treat!=1][1]=wo$ps[wo$treat!=1][1]+1e-7
    bal.plot(wo,var.name = "prop.score", which = "both",
         type = "histogram", mirror = TRUE)
}
coefmat=function(wout, threshold=5){
    if(class(wout)[1]=="weightit"){
        coefs=wout$obj[[1]]$coefficients
    }else{
        coefs=coef(wout,s="lambda.min")%>%as.matrix%>%(function(x){x[x!=0,]%>%t%>%t})
    }
    covup=cbind(NA,apply(wout$data[(coefs%>%rownames)[-1]]%>%as.matrix,2,function(x){quantile(x,c(0, 1),na.rm=T)}))%>%t
    stdcov=c(NA,apply(wout$data[(coefs%>%rownames)[-1]]%>%as.matrix,2,function(x){sd(x,na.rm=T)}))
    res=cbind(coefficient=coefs,standardized_coefficient=coefs*stdcov,covup)[c(T,abs(coefs*stdcov)[-1])>threshold,]
    colnames(res)=c("coefficient", "standardized.coefficient", "covariate.min","covariate.max")
    res
}
plot_ipw_lasso<-function(datestamp,cvl=NULL, filestamp=""){
    if(is.null(cvl)){cvl <- readRDS(paste0("output/ipw_lasso_",filestamp, datestamp,".rds"))}
    ps<-cvl$ps
    sampleweights=cvl$weights
               
# weighted average of outcome for treated municipalities
average.T <- apply(cvl$outcome[cvl$data$treat == 1, ], 2, weighted.mean, w = sampleweights[cvl$data$treat == 1], na.rm=T)
# non-weighted average of outcome for control municipalities
average.C <- apply(cvl$outcome[cvl$data$treat == 0, ], 2, mean, na.rm=T)
# non-weighted average of outcome for treated municipalities
average.all.T <- apply(cvl$outcome[cvl$data$treat == 1, ], 2, mean, na.rm=T)

# estimate the ATC for every day from one week before (April 3) to three weeks after (May 1)
part <- cvl$part
part.adj <- part - 51 
cvl$data[,part] <- (10^5)*cvl$data[, part]/cvl$data$pop.2020
varlist <- names(cvl$data)[part]

d.w <- svydesign(~1, weights = sampleweights, data = cvl$data)

# Variables list
models <- lapply(varlist, function(x){
  svyglm(as.formula(paste0(x," ~ shutdown.",datestamp)), design = d.w)
})

summary.IPW.out <- matrix(NA, length(part), 3)
for(p in 1:length(part)){
  # Coefficient, 2.5, 97.5
  summary.IPW.out[p,1] <- coef(summary(models[[p]]))[2,1]
  summary.IPW.out[p,2] <- confint(models[[p]])[2,1]
  summary.IPW.out[p,3] <- confint(models[[p]])[2,2]
}
min.LB = min(summary.IPW.out[,2])
max.UB = max(summary.IPW.out[,3])
YMD=cvl$YMD
plot(YMD[part.adj], average.C[part.adj], col = 2, lty = 2,
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     xlab="Day", ylab="Outcome",
     main = paste0("Outcome (",format(as.Date(datestamp,"%m%d"),"%B %d"),")"),
     type="l",
     xaxt="n",
     ylim=c(0,
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj]) - min(0,min.LB)))
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
lines(YMD[part.adj], average.C[part.adj], col = 2, lty = 1)
lines(YMD[part.adj], average.T[part.adj], lty = 1)
lines(YMD[part.adj], average.all.T[part.adj], lty = 2)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4

plot(YMD[part.adj], summary.IPW.out[, 1],
     xlim=c(YMD[part.adj[1]], YMD[part.adj[length(part.adj)]]),
     ylim=c(min(0,min.LB),
            max(average.all.T[part.adj], average.C[part.adj], average.T[part.adj], max.UB)),
     xlab="Day", ylab="ATC",
     main = paste0("ATC (",format(as.Date(datestamp,"%m%d"),"%B %d"),")"),
     type="l", xaxt="n")
axis(1, at = c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]),
     labels = format(c(YMD[part.adj[1]], YMD[part.adj[8]], YMD[part.adj[15]], YMD[part.adj[22]], YMD[part.adj[29]]), "%b %d"))
polygon(c(YMD[part.adj], rev(YMD[part.adj])), c(summary.IPW.out[, 3], rev(summary.IPW.out[, 2])), col = "gray90", border = "gray90")
lines(YMD[part.adj], summary.IPW.out[, 1], col = 1, lty = 1, lwd = 3)
abline(v=YMD[part.adj[8]], col = "turquoise") # March 4
abline(0,0,lty=2)
    plot(cvl,main="Cross validation\n\n",sub="number of variables")
}