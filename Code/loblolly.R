> library(nlme)
> fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
+             data = Loblolly,
+             fixed = Asym + R0 + lrc ~ 1,
+             random = Asym ~ 1,
+             start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
> summary(fm1)
Nonlinear mixed-effects model fit by maximum likelihood
  Model: height ~ SSasymp(age, Asym, R0, lrc) 
 Data: Loblolly 
       AIC      BIC    logLik
  239.4856 251.6397 -114.7428

Random effects:
 Formula: Asym ~ 1 | Seed
            Asym  Residual
StdDev: 3.650642 0.7188625

Fixed effects: Asym + R0 + lrc ~ 1 
         Value Std.Error DF   t-value p-value
Asym 101.44960 2.4616951 68  41.21128       0
R0    -8.62733 0.3179505 68 -27.13420       0
lrc   -3.23375 0.0342702 68 -94.36052       0
 Correlation: 
    Asym   R0    
R0   0.704       
lrc -0.908 -0.827

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-2.23601930 -0.62380854  0.05917466  0.65727206  1.95794425 

Number of Observations: 84
Number of Groups: 14 
> fm2 <- update(fm1, random = pdDiag(Asym + lrc ~ 1))
> summary(fm2)
Nonlinear mixed-effects model fit by maximum likelihood
  Model: height ~ SSasymp(age, Asym, R0, lrc) 
 Data: Loblolly 
       AIC      BIC    logLik
  238.9662 253.5511 -113.4831

Random effects:
 Formula: list(Asym ~ 1, lrc ~ 1)
 Level: Seed
 Structure: Diagonal
            Asym        lrc  Residual
StdDev: 2.806185 0.03449969 0.6920003

Fixed effects: Asym + R0 + lrc ~ 1 
         Value Std.Error DF   t-value p-value
Asym 101.85205 2.3239828 68  43.82651       0
R0    -8.59039 0.3058441 68 -28.08747       0
lrc   -3.24011 0.0345017 68 -93.91167       0
 Correlation: 
    Asym   R0    
R0   0.727       
lrc -0.902 -0.796

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-2.06072906 -0.69785679  0.08721706  0.73687722  1.79015782 

Number of Observations: 84
Number of Groups: 14 
