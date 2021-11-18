

attach(data_NHIS)

dat_use <- subset(data_NHIS, AGE >= 25 & AGE <= 65 & EMPHI == "yes workpplace offer health insurance" & EMPFT == "fulltime" & HINOTCOVE == "has health insurance coverage" & BMICALC != "NA") # 
detach()
attach(dat_use)

plot(AGE, BMICALC, main="Age-BMI")
abline(model2, col=3, lwd=5)

model2 <- lm(BMICALC ~ AGE)
summary(model2)

confint(model2)

#logit for men and BMI
data_NHIS$SEX <- (data_NHIS$SEX == "male")
is.na(data_NHIS$SEX) <- which(data_NHIS$SEX == "NA") 


model_logit1 <- glm(SEX ~ BMICALC,
                    family = binomial, data = dat_use)
summary(model_logit1)



#output

Call:
lm(formula = BMICALC ~ AGE)

Residuals:
     Min       1Q   Median       3Q      Max 
-11.5872  -3.9253  -0.8791   3.1805  22.8289 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 26.062051   0.223858 116.422   <2e-16 ***
AGE          0.046189   0.004857   9.509   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.38 on 9440 degrees of freedom
Multiple R-squared:  0.009488,	Adjusted R-squared:  0.009384 
F-statistic: 90.43 on 1 and 9440 DF,  p-value: < 2.2e-16

> 
> confint(model2)
                  2.5 %      97.5 %
(Intercept) 25.62324162 26.50086037
AGE          0.03666801  0.05571036



Call:
glm(formula = SEX ~ BMICALC, family = binomial, data = dat_use)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-1.178  -1.126  -1.083   1.225   1.346  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)   
(Intercept)  0.191682   0.109521   1.750   0.0801 . 
BMICALC     -0.011482   0.003828  -2.999   0.0027 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 13049  on 9441  degrees of freedom
Residual deviance: 13040  on 9440  degrees of freedom
AIC: 13044

Number of Fisher Scoring iterations: 3
