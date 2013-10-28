################################################################################
library(nlme)
head(Machines)
names(Machines)
attach(Machines)
lm(score~Worker)
fit1<-lme(score~Machine, data=Machines, random = ~ 1)
fit2<-lme(score~Machine, data=Machines, random = ~ 1|Worker)
fit3<-lme(score~Machine, data=Machines, random = ~ 1|Worker/Machine)
fit4<-lme(score~Machine, data=Machines, random = ~ Machine -1|Worker)
summary(fit1)
summary(fit2)
summary(fit3)


Machine1 <- Machines[Machines$Worker=="1",]
model.matrix(score ~ Machine, Machine1)
model.matrix(~Machine,Machine1)
model.matrix(~Machine-1,Machine1)
model.matrix(~Machine-1,Machine1)
model.matrix(~1,Machine1)

detach(Machines)
################################################################################
attach(Rail)
head(Rail)
detach(Machines)
