######### More Examples on Mixed Effects Models ###########
library(nlme)
library(MASS)
# LME model fit to the Mental Distress Data given in Chapter 1
dep.dat <- read.table("dep3.dat",head=T)  # get data
attach(dep.dat)
# part of data 
...
     ID month group gender depression
111 194  0.05     2      2  0.6666667
112 194  0.10     2      2  1.6666667
113 195  0.00     2      2  1.8333333
114 195  0.05     2      2  2.3333333
...
# group data based on clusters
dep1.dat <- groupedData(depression ~ month | ID, data=dep.dat) 
# LME model fits
lme1 <- lme(fixed=depression ~ month, random = ~ month,data=dep1.dat)
summary(lme1)
...
Fixed effects: depression ~ month 
                 Value  Std.Error  DF   t-value p-value
(Intercept)  1.4015517 0.05442483 728 25.752064       0
month       -0.4076756 0.05886816 728 -6.925231       0
... 
# We see that the slope is negative and significant, which indicates 
# that depression decreases over time 

# Let's add covariate "group" and "gender" 
lme2 <- lme(fixed=depression ~ month+group+gender, random = ~ month,data=dep1.dat)
summary(lme2)
......
Fixed effects: depression ~ month + group + gender 
                 Value  Std.Error  DF   t-value p-value
(Intercept)  0.8748624 0.22853953 728  3.828057  0.0001
month       -0.4075811 0.05904951 728 -6.902362  0.0000
group       -0.0992603 0.09957314 245 -0.996858  0.3198
gender       0.4004138 0.10351499 245  3.868172  0.0001
# Depression level differs significantly between male and female over time 
# (i.e., significant "gender" effect), but the treatment group does 
# not seem to have a signficant effect over time. 

# GLMM model fit
# Now, let's convert the depression score into a binary variable (i.e., either 
# 1 or 0, depending on whether depression is greater or smaller than average)
dep2 <- as.numeric(dep.dat$depression > mean(dep.dat$depression))
# First, let's try GLM fit, which ignores longitudinal correlation 
# and treat all data as i.i.d. 
glm1 <- glm(dep2~month, family=binomial, data = dep.dat)
summary(glm1)
...
Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.004154   0.078128  -0.053 0.957596    
month       -0.723709   0.193432  -3.741 0.000183 ***
# The above results may be biased since the correlation is ignored.

# Next, let's try a GLMM fit which uses random effects to incorporate 
# longitudinal correlation over time. Note that the R function glmmPQL 
# is based on an approximate method for GLMMs.  
# Case 1: random effect on the intercept 
glmm1 <- glmmPQL(dep2 ~ month, random = ~ 1 | ID, family = binomial, data = dep.dat)
summary(glmm1)
...
Fixed effects: dep2 ~ month 
                 Value Std.Error  DF   t-value p-value
(Intercept)  0.0261358 0.1461217 728  0.178863  0.8581
month       -0.9705548 0.2021263 728 -4.801725  0.0000
......
# We see that the estimates differ from GLM fit, when correlation is incorporated. 

# Case 2: random effects on both parameters 
glmm2 <- glmmPQL(dep2 ~ month, random = ~ 1+month | ID,family = binomial, data = dep.dat)
summary(glmm2)
Fixed effects: dep2 ~ month 
                 Value Std.Error  DF   t-value p-value
(Intercept)  0.0280688 0.1663374 728  0.168746  0.8660
month       -1.0997344 0.2841096 728 -3.870810  0.0001

#### Example:  NLME models for the AIDS data ####
# We return to the HIV dataset described in Chapter 1. We only consider 
# the first 90 day data. The viral load data "lgcopy" (response) is 
# log10-transformed RNA (to make RNA data more normally distributed).  
dat0 <- read.table("aids.dat2",head=T)
dat1 <- dat0[dat0$day<=90, ]   # use only first 90-day data
dat2 <- dat1[!apply(is.na(dat1),1,any),]  # remove missing data 
# A nonlinear function: a bi-exponential viral dynamic model
logexp2<-function(p1,b1,p2,b2,t) log10(exp(p1-b1*t)+exp(p2-b2*t)) 
# Nonlinear model fit, assuming i.i.d. data 
start0 <- c(p1=10,b1=0.5,p2=6,b2=0.005)  # starting value
nls.fit <- nls(lgcopy ~ logexp2(p1,b1,p2,b2,day), data =dat2, start=start0)
summary(nls.fit)
Parameters:
    Estimate Std. Error t value Pr(>|t|)    
p1 12.203337   0.325629  37.476  < 2e-16 ***
b1  0.399031   0.051894   7.689 2.50e-13 ***
p2  7.701700   0.329296  23.388  < 2e-16 ***
b2  0.025750   0.005751   4.478 1.10e-05 ***
.......
# Next, let's treat the data as longitudinal (or grouped) data 
aids.dat <- groupedData(lgcopy ~ day | patid, data=dat2)
# A NLME model fit, with random effects on all 4 parameters 
start <- c(10,0.5,6,0.005)  # starting value 
nlme.fit <- nlme(lgcopy ~ logexp2(p1,b1,p2,b2,day),
                 fixed = p1+b1+p2+b2 ~1,random = p1+b1+p2+b2 ~1, 
                 data =aids.dat,start=c(start)) 
summary(nlme.fit)
...
Fixed effects: p1 + b1 + p2 + b2 ~ 1 
       Value  Std.Error  DF  t-value p-value
p1 12.343397 0.21256889 236 58.06775   0e+00
b1  0.423283 0.02466042 236 17.16447   0e+00
p2  7.547257 0.27357480 236 27.58754   0e+00
b2  0.019879 0.00551205 236  3.60641   4e-04
# We see that standard errors are much reduced. 
