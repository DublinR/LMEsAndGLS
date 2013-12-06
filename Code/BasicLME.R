fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)
#################################################
# fitted()
fm1 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
fitted(fm1, level = 0:1)

#################################################

library(nlme)
library(lattice)

data(Orthodont)
help(Orthodont)
head(Orthodont,n=20)
attach(Orthodont)

xyplot(distance~age|Subject,
	panel=function(x,y){
		panel.lmline(x,y,type="l")
		panel.xyplot(x,y,col=1,pch=16)})

age.means <- tapply(distance,age,mean)
plot(c(8,14),c(20,28),xlab="Age",ylab="Mean distance",pch=16,type="n")
points(c(8,10,12,14),age.means,type="b",col=1,pch=16)

I <- Sex=="Male"
age.means.m <- tapply(distance[I],age[I],mean)
points(c(8,10,12,14),age.means.m,pch=16,col=4)
I <- Sex=="Female"
age.means.m <- tapply(distance[I],age[I],mean)
points(c(8,10,12,14),age.means.m,pch=16,col=3)
legend("topleft",legend=c("Males","Females","Combined"),pch=16,col=c(4,3,1))
###################################################

lmList.obj <- lmList(distance~age|Subject,data=Orthodont) 
coef(lmList.obj)
plot(intervals(lmList.obj))
plot(coef(lmList.obj)[,2],(coef(lmList.obj)[,1]),xlab= "Estimated slope",ylab="Estimated intercept") 

lmList.obj <- lmList(distance~I(age-11)|Subject,data=Orthodont)
coef(lmList.obj)
plot(intervals(lmList.obj))

lm.obj<-lm(distance~age+Sex,data=Orthodont)
summary(lm.obj)
AIC(lm.obj)

lme.obj <- lme(distance~I(age-11)+Sex,data=Orthodont,random=~1|Subject)
lme.obj$coefficients  # gives estimated fixed and predicted random effects
