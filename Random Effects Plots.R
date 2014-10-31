model_lmer<-lmer(distance~age+Sex+(1|Subject),data=Orthodont)
model_lme<-lme(distance~age+Sex, random=~1|Subject,data=Orthodont)

# Random effects plots
# Two different approaches to the plotting of the random effects can be obtained through 
# the following lines of code:

plot(ranef(model_lme))
qqmath(ranef(model_lmer))


# Residuals plots
# lme allows to plot the residuals in the following ways:

res_lme=residuals(model_lme)
plot(res_lme)
qqnorm(res_lme)
qqline(res_lme)
plot(model_lme)
