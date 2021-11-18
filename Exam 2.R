

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

