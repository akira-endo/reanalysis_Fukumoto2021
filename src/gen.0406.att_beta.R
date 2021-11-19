################################
# Genetic Matching 04.06 (ATT) #
################################

data <- read.csv("../Fukumoto2021/covid.csv")

##
# 1. Preprocess
##

source(p_path)

##
# 2. Estimate ATT
##

# Read in Genetic Matching File
m.out <- readRDS(m_path)

# Assess balance
#summary(m.out, un = FALSE)

# Create matched dataset
gm <- get_matches(m.out, data = data.analysis)

# estimate the ATC for every day from one week before (March 30) to three weeks after (April 27)
part <- 120:148 # indicates March 30 to April 27
part.adj <- part - 54
gm[,part] <- (10^5)*gm[, part]/gm$pop.2020

## Null cases from day [nullfrom]-th day
#gm[1:(nrow(gm)/2)*2-1,part[nullfrom:length(part)]]<-gm[1:(nrow(gm)/2)*2,part[nullfrom:length(part)]]*(1-reduction)
orig<-as.matrix(gm[1:(nrow(gm)/2)*2,part[nullfrom:length(part)]])*gm$pop.2020[1:(nrow(gm)/2)*2]/1e5
#newr<-matrix(rnbinom(length(orig),1+orig*gm[1:(nrow(gm)/2)*2-1,]$pop.2020/1e5,mu=(1+orig*gm[1:(nrow(gm)/2)*2-1,]$pop.2020/1e5)/(1/orig+gm[1:(nrow(gm)/2)*2-1,]$pop.2020/1e5)*(1-reduction)),nrow(orig))/(gm[1:(nrow(gm)/2)*2-1,]$pop.2020/1e5)
newr<-pmax(matrix(rtruncnorm(length(orig),0,Inf,orig,sqrt(orig)),nrow(orig)),0,na.rm=T)*(1-reduction)
newr<-pmax(matrix(rtruncnorm(length(orig),0,Inf,newr,sqrt(newr)),nrow(orig)),0,na.rm=T)
gm[1:(nrow(gm)/2)*2-1,part[nullfrom:length(part)]]<-newr/gm$pop.2020[1:(nrow(gm)/2)*2-1]*1e5
##

summary.Match.out2 <- matrix(NA, length(part), 2)
for(p in 1:length(part)){
  fit <- lm(gm[, part[p]] ~ shutdown.0406,
            data = gm, weights = weights)
  summary.Match.out2[p,1] <- coef(summary(fit))[2,1]
  summary.Match.out2[p,2] <- coeftest(fit, vcov. = vcovCL, cluster = ~subclass + id)[2,2]
}

# the upper bound of the 95% confidence interval
UB2 <- summary.Match.out2[, 1] + (summary.Match.out2[, 2] * qnorm(0.975))
# the lower bound of the 95% confidence interval
LB2 <- summary.Match.out2[, 1] + (summary.Match.out2[, 2] * qnorm(0.025))
min.LB2 <- min(LB2, na.rm=T)
max.UB2 <- max(UB2, na.rm=T)

# average of outcome for matched and treated municipalities
average.T2 <- apply(gm[gm$shutdown.0406 == 1, part], 2, mean, na.rm = T)
# average of outcome for matched and control municipalities
average.C2 <- apply(gm[gm$shutdown.0406 == 0, part], 2, mean, na.rm = T)
# average of outcome for treated municipalities (either matched or not)
average.all.T2 <- apply(outcome[treat == 1, ], 2, mean, na.rm=T)
# average of outcome for control municipalities (either matched or not)
average.all.C2 <- apply(outcome[treat == 0, ], 2, mean, na.rm=T)

