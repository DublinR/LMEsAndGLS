fm1 <- lme(distance ~ age, data = Orthodont, random = ~age)
VarCorr(fm1)
# Subject = pdLogChol(age) 
#             Variance   StdDev    Corr  
# (Intercept) 5.41508713 2.3270340 (Intr)
# age         0.05126954 0.2264278 -0.609
# Residual    1.71620401 1.3100397  
