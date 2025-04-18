# Loading Necessary Libraries
library(readr)
library(MASS)
library(Metrics)
library(pROC)


# Assigning Dataset to a Variable
Diabetes <- read_csv("Desktop/diabetes_binary_health_indicators_BRFSS2015.csv")
attach(Diabetes)

# Testing to determine if it is best to treat MentHlth, PhysHlth, Age, and Income
#as Ordinal Variables using Likelihood-ratio Test

# Creating Full Model with all predictors as factors
full_model <- glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
                    BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
                  + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
                  + factor(NoDocbcCost) + factor(GenHlth) + factor(MentHlth) + factor(PhysHlth)
                  + factor(DiffWalk) + factor(Sex) + factor(Age) + factor(Education) + factor(Income)
                  , family=binomial, data=Diabetes)



simple_model <- glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
                      BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
                    + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
                    + factor(NoDocbcCost) + factor(GenHlth) + MentHlth + PhysHlth
                    + factor(DiffWalk) + factor(Sex) + Age + factor(Education) + Income
                    , family=binomial, data=Diabetes)
anova(simple_model,full_model,test="LRT")

# Null Hypothesis: The complex model does not have a better fit
# Alternative Hypothesis: The complex model does have a better fit
# Test Statistic: X^2 = 161742 - 160703 = 1039, df = 75
pchisq(1039,75,lower.tail=FALSE)
# p-value = 1.52 x 10^-169 < 0.05 --> Reject the Null Hypothesis
# At the a=0.05 significance level, we reject the null hypothesis and conclude
#that the complex model does have a better fit

# Need to test every variable individually

# Variable #1: MentHlth
fit1 <-  glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
                            BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
                          + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
                          + factor(NoDocbcCost) + factor(GenHlth) + MentHlth + factor(PhysHlth)
                          + factor(DiffWalk) + factor(Sex) + factor(Age) + factor(Education) + factor(Income)
                          , family=binomial, data=Diabetes)
anova(fit1,full_model,test="LRT")

# Null Hypothesis: The complex model does not have a better fit
# Alternative Hypothesis: The complex model does have a better fit
# Test Statistic: X^2 = 160772 - 160703 = 69, df = 29
pchisq(69,29,lower.tail=FALSE)
# p-value = 4.149814 x 10^-0.05 < 0.05 --> Reject the Null Hypothesis
# At the a=0.05 significance level, we reject the null hypothesis and conclude
#that the complex model does have a better fit
# Keep MentHlth as it is

# Variable #2: PhysHlth
fit2 <-  glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
               BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
             + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
             + factor(NoDocbcCost) + factor(GenHlth) + factor(MentHlth) + PhysHlth
             + factor(DiffWalk) + factor(Sex) + factor(Age) + factor(Education) + factor(Income)
             , family=binomial, data=Diabetes)
anova(fit2,full_model,test="LRT")

# Null Hypothesis: The complex model does not have a better fit
# Alternative Hypothesis: The complex model does have a better fit
# Test Statistic: X^2 = 160743 - 160703 = 40, df = 29
pchisq(40,29,lower.tail=FALSE)
# p-value = 0.0839369 > 0.05 --> Fail to Reject the Null Hypothesis
# At the a=0.05 significance level, we fail reject the null hypothesis and conclude
#that the complex model does not have a better fit
# Change PhysHlth

# Variable #3: Age
fit3 <-  glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
               BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
             + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
             + factor(NoDocbcCost) + factor(GenHlth) + factor(MentHlth) + PhysHlth
             + factor(DiffWalk) + factor(Sex) + Age + factor(Education) + factor(Income)
             , family=binomial, data=Diabetes)
anova(fit3,full_model,test="LRT")

# Null Hypothesis: The complex model does not have a better fit
# Alternative Hypothesis: The complex model does have a better fit
# Test Statistic: X^2 = 161662 - 160703 = 959, df = 40
pchisq(959,40,lower.tail=FALSE)
# p-value = 4.199573 x 10^-175 < 0.05 --> Reject the Null Hypothesis
# At the a=0.05 significance level, we  reject the null hypothesis and conclude
#that the complex model does have a better fit
# Keep Age the way it is

# Variable #4: Income
fit4 <-  glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
               BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
             + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
             + factor(NoDocbcCost) + factor(GenHlth) + factor(MentHlth) + PhysHlth
             + factor(DiffWalk) + factor(Sex) + factor(Age) + factor(Education) + Income
             , family=binomial, data=Diabetes)
anova(fit4,full_model,test="LRT")

# Null Hypothesis: The complex model does not have a better fit
# Alternative Hypothesis: The complex model does have a better fit
# Test Statistic: X^2 = 160774 - 160703 = 71, df = 35
pchisq(71,35,lower.tail=FALSE)
# p-value = 0.0003044499 < 0.05 --> Reject the Null Hypothesis
# At the a=0.05 significance level, we  reject the null hypothesis and conclude
#that the complex model does have a better fit
# Keep Income the way it is

# Only variable that needs to be changed is PhysHlth

changed_model <- glm(Diabetes_binary ~ HighBP + HighChol + factor(CholCheck)+
                         BMI + factor(Smoker) + factor(Stroke) + factor(HeartDiseaseorAttack) + factor(PhysActivity)
                       + factor(Fruits) + factor(Veggies) + factor(HvyAlcoholConsump) + factor(AnyHealthcare)                    
                       + factor(NoDocbcCost) + factor(GenHlth) + factor(MentHlth) + PhysHlth
                       + factor(DiffWalk) + factor(Sex) + factor(Age) + factor(Education) + factor(Income)
                       , family=binomial, data=Diabetes)

# Current AIC = 160893.3

# Using changed_model and conducting Step AIC to build Final Model
final_model <- stepAIC(changed_model) # Removed NoDocbcCost

summary(final_model) # Final AIC = 160892
# Not a major difference compared to initial model built

# Printing ROC and AUC Plot
rocplot <- roc(Diabetes_binary ~ fitted(final_model), data=Diabetes)
plot.roc(rocplot, legacy.axes=TRUE) 
auc(rocplot) # AUC = 0.8246

# Calculating Accuracy, Sensitivity, and Specifity
prop = 0.5

predicted <- as.numeric(fitted(final_model) > prop) 

xtabs(~ Diabetes$Diabetes_binary + predicted)

prop = sum(Diabetes$Diabetes_binary)/nrow(Diabetes)

predicted <- as.numeric(fitted(final_model) > prop) 

table <- xtabs(~ Diabetes$Diabetes_binary + predicted)
print(table)

table[1]
table[2]
table[3]
table[4]

accuracy <-(table[1]+table[4])/sum(table)
print(accuracy) # 0.7264743 ≈ 72.65%

sens <- (table[4])/(table[2]+table[4])
print(sens) # 0.7774288 ≈ 77.74%

spec <- (table[1])/(table[1]+table[3])
print(spec) # 0.7182253 ≈ 71.82%
